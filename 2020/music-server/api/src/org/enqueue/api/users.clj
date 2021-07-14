(ns org.enqueue.api.users
  (:require [org.enqueue.api.db :as db]
            [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.transit :as transit]
            [org.enqueue.api.helpers :refer [date-compare]]
            [org.enqueue.api.router.middleware :refer [wrap-async]]
            [clojure.string :as str])
  (:import [java.time Instant Duration]
           [java.util UUID]))


;; TODO: move this stuff to different namespace?  (org.enqueue.api.auth)
;;   - org.enqueue.api.auth
;;   - org.enqueue.api.auth.middleware
;;   - org.enqueue.api.auth.tokens
;;   - org.enqueue.api.agents
;; TODO: doc strings and comments.
;; TODO: clean and tidy up.
;; TODO: data validation with spec.


;;; ------------------------------------------------------------
;;; EAT tokens
;;;
;;; EAT-A : Enqueue Auth Token - Access
;;; EAT-R : Enqueue Auth Token - Refresh


(defn token-expired? [expires]
  (date-compare > (Instant/now) expires))


(defn- build-payload
  "Build token payload of type :eat-a or :eat-r.  Optionally provide additional
  data to store in the token."
  ([type]
   (assert (#{:eat-a :eat-r} type) "Unsupported token type.")
   (let [now (Instant/now)
         expires (case type
                   :eat-a (.plus now (Duration/ofHours 2))
                   :eat-r (.plus now (Duration/ofDays 400)))]
     {:type    type
      :version "1"
      :expires expires
      :issued  now
      :issuer  "api.enqueue.org"}))
  ([type data]
   (merge data (build-payload type))))


(defn- sign-token [key payload]
  (str payload ":" (crypto/sign-message key payload)))


(defn- package-token [key payload]
  (->> payload
       transit/encode
       crypto/base64-encode
       (sign-token key)))


(defn- build-new-tokens [signing-key user-id agent-id refresh-key]
  (let [base-payload {:user-id user-id, :agent-id agent-id}]
    {:eat-a (->> base-payload
                 (build-payload :eat-a)
                 (package-token signing-key))
     :eat-r (->> (assoc base-payload :refresh-key refresh-key)
                 (build-payload :eat-r)
                 (package-token signing-key))}))


(defn read-token [key token]
  (let [[payload sig] (str/split token #":" 2)]
    (when (crypto/valid-signature? key payload sig)
      (let [data (transit/decode (crypto/base64-decode payload))]
        (when-not (token-expired? (:expires data))
          data)))))


(defn extract-token [key request]
  (when-let [auth-header (get-in request [:headers "Authorization"])]
    (let [[type credentials] (map str/trim (str/split auth-header #"\s+" 2))]
      (when (= type "EAT")
        (read-token key credentials)))))


(defn wrap-auth
  "Wrap handler with token extraction.  If token is invalid or missing,
  responds with HTTP status 401."
  [handler key]
  (fn
    ([request]
     (if-let [token (extract-token request key)]
       (handler (assoc request :token token))
       {:status 401}))
    ([request respond raise]
     (if-let [token (extract-token request key)]
       (handler (assoc request :token token) respond raise)
       (respond {:status 401})))))


;;; ------------------------------------------------------------
;;; Agents
;;;
;;; An agent is automatically logged out after 400 days of inactivity.


(defn- register-agent [user-id {:keys [name version platform idiom]}]
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


(defn- generate-refresh-key []
  (crypto/random-bytes 32))


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
                      (generate-refresh-key)
                      ;; If first log in.
                      (= nil refresh-key (:agents/refresh_key agent))
                      (generate-refresh-key))]
          (do
            (update-rk-db! agent-id rk idempotency-key version)
            {:status 200
             :headers ["Content-Type" transit/content-type]
             :body (transit/encode (build-new-tokens signing-key user-id agent-id rk))})
          (reply 409 "Refresh key already used"))))
    (reply 404 (str "No such agent"))))


;;; ------------------------------------------------------------
;;; Authentication


(defn- find-user-by [& {:keys [id email-address]}]
  (let [query {:select [:id :email-address :password-hash]
               :from [:users]}
        where (cond id [:= :id id]
                    email-address [:= :email-address email-address])]
    (when where
      (db/query-first (assoc query :where where)))))


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


(defn log-in
  [{:keys [email-address password]}
   {:keys [version] :as agent}
   idempotency-key
   signing-key]
  (let [user (find-user-by :email-address email-address)]
    (if (and user
             (crypto/valid-password? (:users/password_hash user) password))
      (let [user-id (:users/id user)
            agent-id (register-agent user-id agent)]
        (create-tokens agent-id version nil idempotency-key signing-key))
      (reply 401 "Invalid credentials"))))


;; TODO: pull actual signing key out of config.
(def _key (crypto/new-signing-key))


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
    (log-in (:credentials body) (:agent body) idempotency-key _key)))


(defn refresh
  [{:keys [agent-id refresh-key]}
   {:keys [version]}
   idempotency-key
   signing-key]
  (create-tokens agent-id version refresh-key idempotency-key signing-key))


(defn refresh-handler
  [request]
  (let [token           (extract-token _key request)
        idempotency-key (get-in request [:headers "Idempotency-Key"])
        body            (transit/decode (:body request))]
    (refresh token (:agent body) idempotency-key _key)))


;; TODO: change password.


(comment
  (def key_ (crypto/new-signing-key))
  (def idempotency-key (UUID/randomUUID))

  (register "alex.vear@enqueue.org" "password")
  (def tokens (log-in {:email-address "alex.vear@enqueue.org"
                       :password "password"}
                      {:name "Enqueue"
                       :platform "Desktop"
                       :idiom "REPL"
                       :version "0"}
                      idempotency-key
                      key_))

  (refresh (read-token key_ (:eat-r (transit/decode (tokens :body))))
           {:version "1"}
           idempotency-key
           key_)
  )


(def user-routes
  [["/account/auth/register" {:post {:handler registration-handler
                                     :middleware [wrap-async]}}]
   ["/account/auth/refresh" {:post {:handler refresh-handler
                                    :middleware [wrap-async]}}]
   ["/account/auth/log-in" {:post {:handler log-in-handler
                                   :middleware [wrap-async]}}]])
