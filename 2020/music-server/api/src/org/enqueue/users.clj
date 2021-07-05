(ns org.enqueue.users
  (:require [org.enqueue.db :as db]
            [org.enqueue.crypto :as crypto]
            [org.enqueue.transit :as transit]
            [org.enqueue.helpers :refer [date-compare]]
            [clojure.string :as str]))


(defn- find-user-by [& {:keys [id email-address]}]
  (let [query {:select [:id :email-address]
               :from [:users]}
        where (cond id [:= :id id]
                    email-address [:= :email-address email-address])]
    (when where
      (db/query-first (assoc query :where where)))))


(defn- create-user [email-address password]
  (if (nil? (find-user-by :email-address email-address))
    (let [user-id (java.util.UUID/randomUUID)
          hashed-password (crypto/hash-password password)]
      (db/insert :users {:id user-id
                         :email-address email-address
                         :password-hash hashed-password
                         :created-at (java.time.Instant/now)})
      user-id)
    (throw (Exception. "User account with that email address already exists."))))


(defn- gen-payload-attrs [type]
  (assert (or (= type :eat-a) (= type :eat-r)))
  {:type    type
   :version "1"
   :expires (java.util.Date/from (.plus (java.time.Instant/now) 2 java.time.temporal.ChronoUnit/HOURS))
   :issued  (java.util.Date/from (java.time.Instant/now))
   :issuer  "api.enqueue.org"})


(defn- sign-token [payload key]
  (str payload ":" (crypto/sign-message key payload)))


(defn cons-token [data type key]
  (-> data
      (merge (gen-payload-attrs type))
      transit/encode
      crypto/base64-encode
      (sign-token key)))


(defn token-expired? [{:keys [expires]}]
  (date-compare <=
                (java.time.Instant/now)
                (.toInstant expires)))


(defn read-token [token key]
  (let [[payload sig] (str/split token #":" 2)]
    (when (crypto/valid-signature? key payload sig)
      (let [data (transit/decode (crypto/base64-decode payload))]
        (when (token-expired? data)
          data)))))


(defn- extract-eat-token [request key]
  (when-let [auth-header (get-in request [:headers "Authorization"])]
    (let [[type credentials] (map str/trim (str/split auth-header #"\s+" 2))]
      (when (= type "EAT")
        (read-token credentials key)))))


(defn wrap-auth
  "Wrap handler with token extraction.  If token is invalid or missing,
  responds with HTTP status 401."
  [handler key]
  (fn
    ([request]
     (if-let [token (extract-eat-token request key)]
       (handler (assoc request :token token))
       {:status 401}))
    ([request respond raise]
     (if-let [token (extract-eat-token request key)]
       (handler (assoc request :token token) respond raise)
       (respond {:status 401})))))

;; TODO: refresh tokens.
;; TODO: token endpoint.


(comment

  ;; EAT-A : Enqueue Auth Token - Access
  ;; EAT-R : Enqueue Auth Token - Refresh  (or session?)
  ;;
  ;; When an EAT-R token is used it generates a new one and revokes the old token.
  ;;
  ;; Session, user, agent, device.
  ;; 1hr life. <- session.
  ;;
  ;; TODO: Idempotence.


  (def key_ (crypto/gen-signing-key))

  (def token
    (cons-token
      {:user-id (java.util.UUID/randomUUID), :agent-id (java.util.UUID/randomUUID)}
      :eat-a
      key_))

  (extract-eat-token {:headers {"Authorization" (str "EAT " token)}} key_)

  )


;; TODO: authentication tokens.
;; TODO: registration.
;; TODO: log in.
;; TODO: auth middleware.
;; TODO: email confirmation.
;; TODO: change password.
;; TODO: reset password.
;; TODO: delete account.
