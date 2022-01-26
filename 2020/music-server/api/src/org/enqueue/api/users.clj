(ns org.enqueue.api.users
  (:require [org.enqueue.api.db     :as db]
            [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.clients.interceptors     :refer [eat-auth-interceptor]]
            [org.enqueue.api.transit.interceptors     :refer [transit-out-interceptor]]
            [org.enqueue.api.idempotency.interceptors :refer [idempotency-interceptor]])
  (:import [java.time Instant]
           [java.util UUID]))


;; TODO: data validation with spec (email addresses).
;; TODO: send emails.


(defn find-user-by [& {:keys [id email-address]}]
  (let [query {:select [:id :email-address :password-hash]
               :from   [:users]}
        where (cond id [:= :id id]
                    email-address [:= :email-address email-address])]
    (when where
      (db/query-first (assoc query :where where)))))


(defn- reply [status message]
  {:status  status
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    message})


(defn register [email-address password]
  (let [email-address (.toLowerCase email-address)]
    (if-not (find-user-by :email-address email-address)
      (let [user-id (UUID/randomUUID)
            hashed-password (crypto/hash-password password)]
        (db/insert! :users {:id            user-id
                            :email-address email-address
                            :password-hash hashed-password
                            :created-at    (Instant/now)})
        (reply 204 "Account created"))
      (reply 400 "User account with that email address already exists"))))


(def registration-handler
  {:name :register-user
   :enter
   (fn [{{{:keys [email-address password]} :body} :request :as context}]
     (assoc context :response
            (if (and email-address password)
              (register email-address password)
              (reply 400 "Invalid body"))))})


(defn change-password [user-id old-password new-password]
  (if-let [user (find-user-by :id user-id)]
    (if (crypto/valid-password? (:users/password_hash user) old-password)
      (let [hashed-password (crypto/hash-password new-password)]
        (db/update! :users [:= :id user-id]
                    {:password-hash hashed-password})
        (reply 204 "Password changed"))
      (reply 401 "Invalid credentials"))
    (reply 404 "User not found")))


(def change-password-handler
  {:name :change-password
   :enter
   (fn [{:keys [request] :as context}]
     (let [user-id      (get-in request [:token :user-id])
           body         (:body request)
           old-password (:old-password body)
           new-password (:new-password body)]
       (assoc context :response
              (change-password user-id old-password new-password))))})


(comment
  (require '[org.enqueue.api.clients     :as clients]
           '[org.enqueue.api.clients.eat :as eat]
           '[org.enqueue.api.config      :as config]
           '[org.enqueue.api.transit     :as transit])

  (def idempotency-key (UUID/randomUUID))

  (register "alex.vear@enqueue.org" "password")

  (def client-creation-response
    (clients/create
      {:email-address "alex.vear@enqueue.org"
       :password      "password"}
      {:name     "Enqueue"
       :platform "Desktop"
       :idiom    "REPL"
       :version  "0"}
      idempotency-key
      config/signing-key))

  (def tokens (-> client-creation-response :body transit/decode))

  (clients/renew
    (eat/read-token config/signing-key (:eat-r tokens))
    {:version "1"}
    idempotency-key
    config/signing-key)

  (clients/revoke-access (:client-id (eat/read-token config/signing-key (:eat-r tokens))))
  )


(def user-routes
  #{["/user/register" :post
     [(idempotency-interceptor)
      transit-out-interceptor
      registration-handler]
     :route-name (:name registration-handler)]
    ["/user/password/change" :post
     [(eat-auth-interceptor)
      (idempotency-interceptor)
      transit-out-interceptor
      change-password-handler]
     :route-name (:name change-password-handler)]})
