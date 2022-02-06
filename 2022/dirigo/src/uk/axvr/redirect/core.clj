(ns uk.axvr.redirect.core
  (:use clojure.pprint)
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [ring.adapter.jetty :refer [run-jetty]]))

(defn read-edn-resource [f]
  (some-> f io/resource slurp edn/read-string))

(defonce redirections
  (atom (read-edn-resource "redirections.edn")))

(defn redirect [request]
  (pprint request)
  (let [host (str (name (:scheme request)) "://" (get-in request [:headers "host"]))
        loc  (get @redirections host)]
    (if loc
      {:status 301  ; 302 307 308
       :headers {"Location" loc}}
      {:status 404})))

(defn app-handler [request]
  (redirect request))

(defn run [{:keys [port block?]
            :or   {port   3000
                   block? true}}]
  (run-jetty
    #'app-handler
    {:port   port
     :join?  (not block?)
     :send-server-version? false}))
