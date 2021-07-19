(ns org.enqueue.api.config
  "Configuration settings for Enqueue."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def read-resource
  (comp eval edn/read-string slurp io/resource))

(def ^:private config
  (read-resource "config.edn"))

(def env (:env config))
(def server (:server config))
(def db (:db config))

(def signing-key (get-in config [:eat :signing-key]))
