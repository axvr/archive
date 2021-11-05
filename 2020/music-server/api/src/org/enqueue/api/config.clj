(ns org.enqueue.api.config
  "Configuration settings for Enqueue."
  (:require [org.enqueue.api.helpers :refer [read-edn-resource]]))

(def ^:private config
  (if-let [conf (read-edn-resource "config.edn")]
    conf
    (throw (ex-info "No configuration file found." {:file "config.edn"}))))

(def env   (:env config))
(def dev?  (= env :dev))
(def test? (= env :test))
(def prod? (= env :prod))

(def server (:server config))
(def db     (:db config))

(def signing-key (get-in config [:eat :signing-key]))
