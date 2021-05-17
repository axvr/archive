(ns org.enqueue.users
  (:require [org.enqueue.db :as db]
            [org.enqueue.crypto :as crypto]
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


(defn gen-payload-attrs [type]
  (assert (or (= type :eat-a) (= type :eat-r)))
  {:type    type
   :version "1"
   :expires (.toString (.plus (java.time.Instant/now) 1 java.time.temporal.ChronoUnit/HOURS))
   :issued  (.toString (java.time.Instant/now))
   :issuer  "api.enqueue.org"})


(defn sign-token [payload key]
  (str payload ":" (crypto/sign-message key payload)))


(defn cons-token [data type key]
  (-> data
      (merge (gen-payload-attrs type))
      str
      crypto/base64-encode
      (sign-token key)))


(defn token-expired? [{:keys [expires]}]
  (> 0 (.compareTo (java.time.Instant/now)
                   (java.time.Instant/parse expires))))


(defn read-token [token key]
  (let [[payload sig] (str/split token #":" 2)]
    (when (crypto/valid-signature? key payload sig)
      (let [data (read-string (crypto/base64-decode payload))]
        (when (token-expired? data)
          data)))))


;; TODO: refresh tokens.
;; TODO: token endpoint.
;; TODO: token middleware.


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


  (def key (crypto/gen-signing-key))

  (read-token
    (cons-token
      {:user-id (java.util.UUID/randomUUID), :agent-id (java.util.UUID/randomUUID)}
      :eat-a
      key)
    key)

  )


;; TODO: authentication tokens.
;; TODO: registration.
;; TODO: log in.
;; TODO: auth middleware.
;; TODO: email confirmation.
;; TODO: change password.
;; TODO: reset password.
;; TODO: delete account.
