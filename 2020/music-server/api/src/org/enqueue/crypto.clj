(ns org.enqueue.crypto
  (:require [caesium.crypto.pwhash :as pwhash]
            [clojure.string :as string]))


(defn- encode
  "Encodes a string using Base64."
  [s]
  (.encodeToString (java.util.Base64/getEncoder) (.getBytes s)))


(defn- decode
  "Decodes a Base64 encoded string."
  [s]
  (String. (.decode (java.util.Base64/getDecoder) s) "UTF-8"))


(defn hash-password
  "Hashes a user password using libsodium's pwhash API.  The returned hash is
  a Base64 encoded UTF-8 string.  The Base64 encoding is performed so that the
  hash can be stored in a UTF-8 encoded PostgreSQL database which rejects the
  NUL terminated strings returned by libsodium (a C library)."
  [password]
  (encode
    (pwhash/pwhash-str password
                       pwhash/opslimit-sensitive
                       pwhash/memlimit-sensitive)))


(defn verify-password
  "Checks if a given password matches a Base64 encoded password hash.  Used for
  authentication and testing if initial password hashing was successful."
  [password-hash password]
  (= 0 (pwhash/pwhash-str-verify
         (decode password-hash)
         password)))
