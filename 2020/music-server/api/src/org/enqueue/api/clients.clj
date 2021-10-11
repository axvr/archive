(ns org.enqueue.api.clients
  "Clients are the devices connected to a user's account that users interact
  with Enqueue through.

  A client using EAT is automatically logged out after 400 days of inactivity."
  (:require [org.enqueue.api.db          :as db]
            [org.enqueue.api.users       :as users]
            [org.enqueue.api.crypto      :as crypto]
            [org.enqueue.api.clients.eat :as eat]
            [org.enqueue.api.config      :as config]
            [org.enqueue.api.clients.middleware     :refer [wrap-auth]]
            [org.enqueue.api.router.middleware      :refer [wrap-async]]
            [org.enqueue.api.transit.middleware     :refer [wrap-transit]]
            [org.enqueue.api.idempotency.middleware :refer [wrap-idempotent
                                                            wrap-idempotency-key]])
  (:import [java.util UUID]
           [java.time Instant]))


(defn- reply [status message]
  {:status  status
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    message})


(defn- update-stored-renewal-key! [client-id renewal-key idempotency-key version]
  (db/update! :clients [:= :id client-id]
              {:renewal-key     renewal-key
               :idempotency-key idempotency-key
               :version         version
               :last-session    (Instant/now)}))


(defn- create-tokens [client-id version renewal-key idempotency-key signing-key]
  (if-let [client (db/query-first
                   {:select [:user-id :renewal-key :idempotency-key :access-revoked]
                    :from   [:clients]
                    :where  [:= :id client-id]})]
    (let [user-id         (:clients/user_id client)
          access-revoked? (:clients/access_revoked client)]
      (if access-revoked?
        (reply 403 "Access revoked")
        (if-let [rk (cond
                      ;; If idempotency key (and renewal key) was given and
                      ;; matches stored key.
                      (and renewal-key
                           idempotency-key
                           (= idempotency-key (:clients/idempotency_key client)))
                      (:clients/idempotency_key client)
                      ;; If renewal key was given and matches stored key.
                      (and renewal-key
                           (= renewal-key (:clients/renewal_key client)))
                      (eat/generate-renewal-key)
                      ;; If first log in.
                      (= nil renewal-key (:clients/renewal_key client))
                      (eat/generate-renewal-key))]
          (do
            (update-stored-renewal-key! client-id rk idempotency-key version)
            {:status 200
             :body   (eat/build-token-pair signing-key user-id client-id rk)})
          (reply 409 "Renewal token already used"))))
    (reply 404 (str "No such client"))))


(defn- create-client [user-id {:keys [name version platform idiom]}]
  ;; TODO: validate client data.  (Spec?)
  (let [client-id (UUID/randomUUID)]
    (db/insert!
      :clients
      {:id         client-id
       :user-id    user-id
       :name       name
       :version    version
       :platform   platform
       :idiom      idiom
       :created-at (Instant/now)
       :access-revoked false})
    client-id))


(defn create
  "Create a new client and provide initial EAT tokens."
  [{:keys [email-address password]}
   {:keys [version] :as client}
   idempotency-key
   signing-key]
  (let [email-address (when email-address (.toLowerCase email-address))
        user (users/find-user-by :email-address email-address)]
    (if (and user
             (crypto/valid-password? (:users/password_hash user) password))
      (let [user-id (:users/id user)
            client-id (create-client user-id client)]
        (create-tokens client-id version nil idempotency-key signing-key))
      (reply 401 "Invalid credentials"))))


(defn create-handler
  "Expects request body to be in following format:
    {:client {:name     \"Enqueue\"
              :platform \"Web\"
              :idiom    \"Desktop\"
              :version  \"1\"}
     :credentials {:email-address \"example@example.com\"
                   :password      \"password\"}}"
  [request]
  (let [idempotency-key (:idempotency-key request)
        body            (:body request)
        credentials     (:credentials body)
        client          (:client body)]
    (create credentials client idempotency-key config/signing-key)))


(defn renew
  "Renew an client's access by providing the currently active EAT-R token.
  Returns a new pair of EAT tokens."
  [{:keys [client-id renewal-key]}
   {:keys [version]}
   idempotency-key
   signing-key]
  (create-tokens client-id version renewal-key idempotency-key signing-key))


(defn renew-handler
  "Renew an client's access by providing the currently active EAT-R token.
  Returns a new pair of EAT tokens.

  This endpoint has its own idempotency implementation as it has higher
  reliability and security requirements; don't want the client to get logged
  out due to a poor network connection."
  [request]
  (let [signing-key     config/signing-key
        token           (:token request)
        idempotency-key (:idempotency-key request)
        client          (get-in request [:body :client])]
    (renew token client idempotency-key signing-key)))


(defn revoke-access
  "Revoke a client's access.  This is somewhat equivalent to logging-out."
  [client-id]
  (db/update! :clients [:= :id client-id]
              {:access-revoked  true
               :renewal-key     nil
               :idempotency-key nil})
  (eat/revoke-client-access client-id))


(defn revoke-access-handler [request]
  (let [client-id (get-in request [:token :client-id])]
    (revoke-access client-id)
    {:status 204}))


(def client-routes
  [["/clients/create" {:post {:handler create-handler
                              :middleware [wrap-idempotent
                                           wrap-transit
                                           wrap-async]}}]
   ["/clients/renew"  {:post {:handler renew-handler
                              :middleware [wrap-idempotency-key
                                           wrap-transit
                                           wrap-auth
                                           wrap-async]}}]
   ["/clients/revoke" {:post {:handler revoke-access-handler
                              :middleware [wrap-idempotent
                                           wrap-transit
                                           wrap-auth
                                           wrap-async]}}]])
