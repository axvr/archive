(ns org.enqueue.api.config
  "Configuration settings for Enqueue."
  (:require [clojure.java.io :as io]))

(def read-resource
  (comp eval read-string slurp io/resource))

(def ^:private config
  (read-resource "config.edn"))

(def env    (config :env))
(def server (config :server))
(def db     (config :db))

(def signing-key (get-in config [:eat :signing-key]))
