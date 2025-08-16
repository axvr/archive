(ns uk.axvr.dirigo.acme
  "Dirigo ACME (Automatic Certificate Management Environment) integration."
  (:require [clojure.java.io :as io])
  (:import  [java.security Security]
            [java.io FileWriter FileReader]
            [org.shredzone.acme4j Session AccountBuilder]
            [org.shredzone.acme4j.util KeyPairUtils]
            [org.bouncycastle.jce.provider BouncyCastleProvider]))

(Security/addProvider (BouncyCastleProvider.))

;; TODO: switch between environments.
(def ^:const let's-encrypt "acme://letsencrypt.org")
(def ^:const let's-encrypt-staging "acme://letsencrypt.org/staging")

;; https://shredzone.org/maven/acme4j/usage/session.html
(defonce session
  (doto (Session. let's-encrypt-staging)
    (.setLocale java.util.Locale/UK)))

(comment
  (.getTermsOfService (.getMetadata session)))

;; https://shredzone.org/maven/acme4j/usage/account.html#creating-an-account-key-pair
(defn load-or-create-key-pair
  [& {:keys [ecdsa-curve key-file]
      :or   {ecdsa-curve "secp256r1"
             key-file    "acme_keypair.pem"}}]
  (let [f (io/file key-file)]
    (if (.exists f)
      (with-open [r (FileReader. f)]
        (println "ACME: Reading key from:" (.getAbsolutePath f))
        (KeyPairUtils/readKeyPair r))
      (with-open [w (FileWriter. f)]
        (println "ACME: Key file not found, creating new key pair")
        (let [key (KeyPairUtils/createECKeyPair ecdsa-curve)]
          (println "ACME: Writing key to:" (.getAbsolutePath f))
          (KeyPairUtils/writeKeyPair key w)
          key)))))

(comment
  ;; Seems that Let's Encrypt don't support EdDSA, so need to pick a decent
  ;; ECDSA curve.  But which one?  https://safecurves.cr.yp.to

  ;; View list of supported ECDSA curves supported by BouncyCastle:
  (enumeration-seq (org.bouncycastle.jce.ECNamedCurveTable/getNames))

  ;; If you use an curve not supported by Let's Encrypt, account creation will
  ;; fail with "Unknown EC name ...".
  )

;; https://shredzone.org/maven/acme4j/usage/account.html#register-an-account
(defn create-account [session key-pair email-addr]
  (.. (AccountBuilder.)
      (addContact (format "mailto:%s" email-addr))
      (agreeToTermsOfService)
      (useKeyPair key-pair)
      (create session)))

;; https://shredzone.org/maven/acme4j/usage/login.html
(defn login [session account key-pair]
  (.login session (.getLocation account) key-pair))

(comment
  (let [key-pair (load-or-create-key-pair)]
    (login
     session
     (create-account session key-pair "infra@axvr.uk")
     key-pair)))

(defn order [account domains]
  (.. account
      (newOrder)
      (domains ["axvr.uk" "ascribe.axvr.uk"])
      (create)))

(defn http01-challenge-handler [request]
  {:headers {"Content-Type" "text/plain"}
   :body nil})
