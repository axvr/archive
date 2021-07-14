(ns org.enqueue.api.settings
  (:require [org.enqueue.api.crypto :as crypto]))

;; TODO: build a proper configuration/settings system.

(def signing-key (crypto/new-signing-key))
