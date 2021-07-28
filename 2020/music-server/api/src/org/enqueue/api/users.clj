(ns org.enqueue.api.users
  (:require [org.enqueue.api.db     :as db]
            [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.agents.middleware      :refer [wrap-auth]]
            [org.enqueue.api.router.middleware      :refer [wrap-async]]
            [org.enqueue.api.transit.middleware     :refer [wrap-transit]]
            [org.enqueue.api.idempotency.middleware :refer [wrap-idempotent]])
  (:import [java.time Instant]
           [java.util UUID]))


;; TODO: data validation with spec (email addresses).
;; TODO: send emails.
;; TODO: make idempotent.


(defn find-user-by [& {:keys [id email-address]}]
  (let [query {:select [:id :email-address :password-hash]
               :from [:users]}
        where (cond id [:= :id id]
                    email-address [:= :email-address email-address])]
    (when where
      (db/query-first (assoc query :where where)))))


(defn- reply [status message]
  {:status status
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body message})


(defn register [email-address password]
  (if-not (find-user-by :email-address email-address)
    (let [user-id (UUID/randomUUID)
          hashed-password (crypto/hash-password password)]
      (db/insert! :users {:id user-id
                          :email-address email-address
                          :password-hash hashed-password
                          :created-at (Instant/now)})
      (reply 204 "Account created"))
    (reply 400 "User account with that email address already exists")))


(defn registration-handler [request]
  (let [{:keys [email-address password]} (:body request)]
    (if (and email-address password)
      (register email-address password)
      (reply 400 "Invalid body"))))


(defn change-password [user-id old-password new-password]
  (if-let [user (find-user-by :user-id user-id)]
    (if (crypto/valid-password? (:users/password_hash user) old-password)
      (let [hashed-password (crypto/hash-password new-password)]
        (db/update! :users [:= :id user-id]
                    {:password-hash hashed-password})
        (reply 204 "Password changed"))
      (reply 401 "Invalid credentials"))
    (reply 404 "User not found")))


(defn change-password-handler [request]
  (let [user-id      (get-in request [:token :user-id])
        body         (:body request)
        old-password (:old-password body)
        new-password (:new-password body)]
    (change-password user-id old-password new-password)))


(comment
  (require '[org.enqueue.api.agents     :as agents]
           '[org.enqueue.api.agents.eat :as eat]
           '[org.enqueue.api.config     :as config]
           '[org.enqueue.api.transit    :as transit])

  (def idempotency-key (UUID/randomUUID))

  (register "alex.vear@enqueue.org" "password")

  (def agent-creation-response
    (agents/create
      {:email-address "alex.vear@enqueue.org"
       :password "password"}
      {:name "Enqueue"
       :platform "Desktop"
       :idiom "REPL"
       :version "0"}
      idempotency-key
      config/signing-key))

  (def tokens (-> agent-creation-response :body transit/decode))

  (agents/renew
    (eat/read-token config/signing-key (:eat-r tokens))
    {:version "1"}
    idempotency-key
    config/signing-key)

  (agents/revoke-access (:agent-id (eat/read-token config/signing-key (:eat-r tokens))))
  )


(def user-routes
  [["/user/register"       {:post {:handler registration-handler
                                   :middleware [wrap-async
                                                wrap-transit
                                                wrap-idempotent]}}]
   ["user/password/change" {:post {:handler change-password-handler
                                   :middleware [wrap-async
                                                wrap-auth
                                                wrap-transit
                                                wrap-idempotent]}}]])
