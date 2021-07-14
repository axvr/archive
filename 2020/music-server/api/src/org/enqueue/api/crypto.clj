(ns org.enqueue.api.crypto
  "Core cryptographic functions for Enqueue."
  (:require [caesium.crypto.pwhash :as pwhash]
            [caesium.crypto.auth :as auth]
            [caesium.randombytes :as rb]))


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
  a Base64 encoded string.  This encoding is done so that the hash can be
  stored in a UTF-8 encoded PostgreSQL database which would otherwise reject
  the ASCII null terminated strings returned by libsodium (a C library)."
  [password]
  (base64-encode
    (pwhash/pwhash-str password
                       pwhash/opslimit-sensitive
                       pwhash/memlimit-sensitive)
    "US-ASCII"))


(defn valid-password?
  "Checks if a given password matches a Base64 encoded password hash.  Used for
  user authentication."
  [password-hash password]
  (= 0 (pwhash/pwhash-str-verify
         (base64-decode password-hash "US-ASCII")
         password)))


(defn- bytebuffer->string
  "Convert the bytebuffers returned by caesium into Java strings."
  ([bb]
   (bytebuffer->string bb "ISO-8859-1"))
  ([bb charset]
   (String. bb charset)))


(defn- string->bytebuffer
  "Convert a Java string into a bytebuffer suitable for use by caesium."
  ([s]
   (string->bytebuffer s "ISO-8859-1"))
  ([s charset]
   (.getBytes s charset)))


(defn random-bytes
  "Generates a random string of specified size and returns it encoded in
  Base64.  Useful for generating cryptographic keys."
  [size]
  (-> size
      rb/randombytes
      bytebuffer->string
      (base64-encode "ISO-8859-1")))


(defn new-signing-key
  "Generate a random Base64 encoded signing key for the HMAC-SHA512-256
  algorithm.  Used by the sign-message and valid-signature? functions."
  []
  (random-bytes auth/hmacsha512256-keybytes))


(defn sign-message
  "Generate a HMAC-SHA512-256 signature for a message using the given key."
  [key msg]
  (let [k (string->bytebuffer (base64-decode key "ISO-8859-1"))]
    (base64-encode
      (bytebuffer->string
        (auth/hmacsha512256 msg k))
      "ISO-8859-1")))


(defn valid-signature?
  "Verify that a HMAC-SHA512-256 generated signature (sig) is valid for the
  given message (msg)."
  [key msg sig]
  (try
    (= sig (sign-message key msg))
    (catch Throwable _
      false)))
