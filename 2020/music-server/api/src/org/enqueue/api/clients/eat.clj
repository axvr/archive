(ns org.enqueue.api.clients.eat
  "EAT (Enqueue authentication tokens)

  The 2 types of tokens are:
    - EAT-A: Access token (TTL 2 hours).
    - EAT-R: Renewal token (TTL 400 days)"
  (:require [org.enqueue.api.crypto     :as crypto]
            [org.enqueue.api.transit    :as transit]
            [uk.axvr.refrain            :as r]
            [clojure.string             :as str]
            [clojure.core.cache.wrapped :as cache])
  (:import [java.time Instant Duration]))


(def eat-a-ttl (Duration/ofHours 2))
(def eat-r-ttl (Duration/ofDays 400))


(defonce
  ^{:doc "Cache of clients with revoked EAT-A tokens (for \"instant log-out\")."
    :private true}
  revoked-clients
  (cache/ttl-cache-factory {} :ttl (.toMillis eat-a-ttl)))


(defn revoke-client-access
  "Revoke all EAT-A tokens for an client."
  [client-id]
  {:pre [(uuid? client-id)]}
  (cache/miss revoked-clients client-id true))


(defn expired?
  "Returns true if an EAT token has expired.  Otherwise returns false."
  [{:keys [expires client-id type]}]
  (or (r/date-compare > (Instant/now) expires)
      (and (= type :eat-a)
           (cache/has? revoked-clients client-id))))


(defn generate-renewal-key []
  (crypto/random-bytes 32))


(defn build-payload
  "Build token payload of type :eat-a or :eat-r.  Optionally provide additional
  data to store in the token."
  ([type]
   {:pre [(#{:eat-a :eat-r} type)]}
   (let [now (Instant/now)
         expires (case type
                   :eat-a (.plus now eat-a-ttl)
                   :eat-r (.plus now eat-r-ttl))]
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


(defn build-token-pair [signing-key user-id client-id renewal-key]
  (let [base-payload {:user-id user-id, :client-id client-id}]
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
        (when-not (expired? data)
          data)))))


(defn extract-token
  "Pulls an EAT token out of the Authorization header and read it."
  [key request]
  (when-let [auth-header (get-in request [:headers "Authorization"])]
    (let [[type credentials] (map str/trim (str/split auth-header #"\s+" 2))]
      (when (= type "EAT")
        (read-token key credentials)))))
