(ns uk.axvr.redirect.core
  (:require [uk.axvr.redirect.rules :as rules]
            [ring.adapter.jetty :refer [run-jetty]])
  (:import java.net.URI))


(defn request->url
  [{:keys [scheme server-name uri query-string method]}]
  {:scheme   scheme
   :host     server-name
   :path     uri
   :query    query-string
   :method   method})


(defn url->response
  [{:keys [type scheme user host port path query fragment]}]
  (if (and scheme host)
    {:status (case type
               :permanent 301
               :temporary 302
               301)
     :headers {"Location"
               (.toString
                 (URI. (name scheme) user host (or port -1) path query fragment))}}
    {:status 421}))


(defn apply-rule [ruleset url]
  (when-let [rule (get ruleset (:host url))]
    (rule url)))


(defn redirector [request]
  (let [ruleset @rules/ruleset]
    (loop [url (request->url request)]
      (if-let [url (apply-rule ruleset url)]
        (recur url)
        (url->response url)))))


(defn run
  [{:keys [port block?]
    :or   {port   8080
           block? true}}]
  (run-jetty
    #'redirector
    {:port   port
     :join?  (not block?)
     :send-server-version? false}))
