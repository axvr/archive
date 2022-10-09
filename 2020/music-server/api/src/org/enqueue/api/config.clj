(ns org.enqueue.api.config
  "Configuration settings for Enqueue."
  (:require [uk.axvr.refrain :as r]))

(def ^:private config
  (or (r/read-edn-resource "config.edn")
      (throw (ex-info "No configuration file found." {:file "config.edn"}))))

(def env   (:env config))
(def dev?  (= env :dev))
(def test? (= env :test))
(def prod? (= env :prod))

(def server (:server config))
(def db     (:db config))

(def signing-key (get-in config [:eat :signing-key]))
