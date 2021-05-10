(ns org.enqueue.crypto
  (:require [caesium.crypto.pwhash :as pwhash]))


(defn base64-encode
  "Encodes a string using Base64.  If no charset is specified, assumes UTF-8."
  ([s]
   (base64-encode s "UTF-8"))
  ([s charset]
   (.encodeToString (java.util.Base64/getEncoder) (.getBytes s charset))))


(defn base64-decode
  "Decodes a Base64 encoded string.  If no charset is specified, assumes
  UTF-8."
  ([s]
   (base64-decode s "UTF-8"))
  ([s charset]
   (String. (.decode (java.util.Base64/getDecoder) s) charset)))


(defn hash-password
  "Hashes a user password using libsodium's pwhash API.  The returned hash is
  a Base64 encoded UTF-8 string.  The Base64 encoding is performed so that the
  hash can be stored in a UTF-8 encoded PostgreSQL database which rejects the
  NUL terminated strings returned by libsodium (a C library)."
  [password]
  (base64-encode
    (pwhash/pwhash-str password
                       pwhash/opslimit-sensitive
                       pwhash/memlimit-sensitive)
    "US-ASCII"))


(defn verify-password
  "Checks if a given password matches a Base64 encoded password hash.  Used for
  authentication and testing if initial password hashing was successful."
  [password-hash password]
  (= 0 (pwhash/pwhash-str-verify
         (base64-decode password-hash "US-ASCII")
         password)))
