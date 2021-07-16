(ns org.enqueue.api.agents
  "Agents are the devices connected to a user's account that users interact
  with Enqueue through.

  An agent using EAT is automatically logged out after 400 days of inactivity."
  (:require [org.enqueue.api.db :as db]
            [org.enqueue.api.users :as users]
            [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.transit :as transit]
            [org.enqueue.api.agents.eat :as eat]
            [org.enqueue.api.config :as config]
            [org.enqueue.api.agents.middleware :refer [wrap-auth]]
            [org.enqueue.api.router.middleware :refer [wrap-async]])
  (:import [java.util UUID]
           [java.time Instant]))


;;; TODO: improve naming.
;;;   - /agents/register
;;;   - /agents/remove
;;;   - /agents/renew
;;;   - /agents
;;; TODO: clean up
;;; TODO: data validation with spec.


(defn- create-agent [user-id {:keys [name version platform idiom]}]
  ;; TODO: validate agent data.  (Spec?)
  (let [agent-id (UUID/randomUUID)]
    (db/insert!
      :agents
      {:id         agent-id
       :user-id    user-id
       :name       name
       :version    version
       :platform   platform
       :idiom      idiom
       :created-at (Instant/now)
       :access-revoked false})
    agent-id))


;; TODO: better name.
(defn- update-rk-db! [agent-id refresh-key idempotency-key version]
  (db/update! :agents [:= :id agent-id]
              {:refresh-key refresh-key
               :idempotency-key idempotency-key
               :version version
               :last-session (Instant/now)}))


(defn- reply [status message]
  {:status status
   :headers ["Content-Type" "text/plain"]
   :body message})


(defn- create-tokens [agent-id version refresh-key idempotency-key signing-key]
  (if-let [agent (db/query-first
                   {:select [:user-id :refresh-key :idempotency-key :access-revoked]
                    :from [:agents]
                    :where [:= :id agent-id]})]
    (let [user-id         (:agents/user_id agent)
          access-revoked? (:agents/access_revoked agent)]
      (if access-revoked?
        (reply 403 "Access revoked")
        (if-let [rk (cond
                      ;; If idempotency key (and refresh key) was given and
                      ;; matches stored key.
                      (and refresh-key
                           idempotency-key
                           (= idempotency-key (:agents/idempotency_key agent)))
                      (:agents/idempotency_key agent)
                      ;; If refresh key was given and matches stored key.
                      (and refresh-key
                           (= refresh-key (:agents/refresh_key agent)))
                      (eat/generate-refresh-key)
                      ;; If first log in.
                      (= nil refresh-key (:agents/refresh_key agent))
                      (eat/generate-refresh-key))]
          (do
            (update-rk-db! agent-id rk idempotency-key version)
            {:status 200
             :headers ["Content-Type" transit/content-type]
             :body (transit/encode (eat/build-token-pair signing-key user-id agent-id rk))})
          (reply 409 "Refresh key already used"))))
    (reply 404 (str "No such agent"))))


(defn log-in
  [{:keys [email-address password]}
   {:keys [version] :as agent}
   idempotency-key
   signing-key]
  (let [user (users/find-user-by :email-address email-address)]
    (if (and user
             (crypto/valid-password? (:users/password_hash user) password))
      (let [user-id (:users/id user)
            agent-id (create-agent user-id agent)]
        (create-tokens agent-id version nil idempotency-key signing-key))
      (reply 401 "Invalid credentials"))))


(defn log-in-handler
  "Expects request body to be in following format:
    {:agent {:name     \"Enqueue\"
             :platform \"Web\"
             :idiom    \"Desktop\"
             :version  \"1\"}
     :credentials {:email-address \"example@example.com\"
                   :password      \"password\"}}"
  [request]
  (let [idempotency-key (get-in request [:headers "Idempotency-Key"])
        body            (transit/decode (:body request))]
    (log-in (:credentials body) (:agent body) idempotency-key config/signing-key)))


(defn refresh
  [{:keys [agent-id refresh-key]}
   {:keys [version]}
   idempotency-key
   signing-key]
  (create-tokens agent-id version refresh-key idempotency-key signing-key))


(defn refresh-handler [request]
  (let [signing-key     config/signing-key
        token           (eat/extract-token signing-key request)
        idempotency-key (get-in request [:headers "Idempotency-Key"])
        body            (transit/decode (:body request))]
    (refresh token (:agent body) idempotency-key signing-key)))


(defn log-out [agent-id]
  (db/update! :agents [:= :id agent-id]
              {:access-revoked  true
               :refresh-key     nil
               :idempotency-key nil}))


(defn log-out-handler [request]
  (let [agent-id (get-in request [:token :agent-id])]
    (log-out agent-id)
    {:status 204}))


(def agent-routes
  [["/agents/register" {:post {:handler log-in-handler
                               :middleware [wrap-async]}}]
   ["/agents/refresh"  {:post {:handler refresh-handler
                               :middleware [wrap-async]}}]
   ["/agents/remove"   {:post {:handler log-out-handler
                               :middleware [wrap-async wrap-auth]}}]])
