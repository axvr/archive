(ns org.enqueue.api.users
  (:require [org.enqueue.api.db :as db]
            [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.transit :as transit]
            [org.enqueue.api.router.middleware :refer [wrap-async]])
  (:import [java.time Instant]
           [java.util UUID]))


;; TODO: data validation with spec.
;; TODO: password changing.


(defn find-user-by [& {:keys [id email-address]}]
  (let [query {:select [:id :email-address :password-hash]
               :from [:users]}
        where (cond id [:= :id id]
                    email-address [:= :email-address email-address])]
    (when where
      (db/query-first (assoc query :where where)))))


(defn- reply [status message]
  {:status status
   :headers ["Content-Type" "text/plain"]
   :body message})


;; TODO: make idempotent.
(defn register [email-address password]
  ;; TODO: validate email-address (use spec?)
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
  (let [{:keys [email-address password]}
        (transit/decode (:body request))]
    (if (and email-address password)
      (register email-address password)
      (reply 400 "Invalid body"))))


;; (comment
;;   (def key_ (crypto/new-signing-key))
;;   (def idempotency-key (UUID/randomUUID))

;;   (register "alex.vear@enqueue.org" "password")
;;   (def tokens (log-in {:email-address "alex.vear@enqueue.org"
;;                        :password "password"}
;;                       {:name "Enqueue"
;;                        :platform "Desktop"
;;                        :idiom "REPL"
;;                        :version "0"}
;;                       idempotency-key
;;                       key_))

;;   (refresh (read-token key_ (:eat-r (transit/decode (tokens :body))))
;;            {:version "1"}
;;            idempotency-key
;;            key_)
;;   )


(def user-routes
  [["/user/register" {:post {:handler registration-handler
                             :middleware [wrap-async]}}]])
