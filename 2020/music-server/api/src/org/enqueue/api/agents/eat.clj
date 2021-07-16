(ns org.enqueue.api.agents.eat
  "EAT (Enqueue authentication tokens)

  The 2 types of tokens are:
    - EAT-A: Access token (TTL 2 hour).
    - EAT-R: Renewal token (TTL 400 days)"
  (:require [org.enqueue.api.crypto :as crypto]
            [org.enqueue.api.transit :as transit]
            [org.enqueue.api.helpers :refer [date-compare]]
            [clojure.string :as str])
  (:import [java.time Instant Duration]))


(defn expired?
  "Returns true if an EAT token has expired.  Otherwise returns false."
  [expires]
  (date-compare > (Instant/now) expires))


(defn generate-renewal-key []
  (crypto/random-bytes 32))


(defn build-payload
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


(defn sign-token [key payload]
  (str payload ":" (crypto/sign-message key payload)))


(defn pack-token [key payload]
  (->> payload
       transit/encode
       crypto/base64-encode
       (sign-token key)))


(defn build-token-pair [signing-key user-id agent-id renewal-key]
  (let [base-payload {:user-id user-id, :agent-id agent-id}]
    {:eat-a (->> base-payload
                 (build-payload :eat-a)
                 (pack-token signing-key))
     :eat-r (->> (assoc base-payload :renewal-key renewal-key)
                 (build-payload :eat-r)
                 (pack-token signing-key))}))


(defn read-token
  "Reads an EAT token and checks if it has expired."
  [key token]
  (let [[payload sig] (str/split token #":" 2)]
    (when (crypto/valid-signature? key payload sig)
      (let [data (transit/decode (crypto/base64-decode payload))]
        (when-not (expired? (:expires data))
          data)))))


(defn extract-token
  "Pulls an EAT token out of the Authorization header and read it."
  [key request]
  (when-let [auth-header (get-in request [:headers "Authorization"])]
    (let [[type credentials] (map str/trim (str/split auth-header #"\s+" 2))]
      (when (= type "EAT")
        (read-token key credentials)))))
