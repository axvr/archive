(ns uk.axvr.dirigo.core
  (:require [uk.axvr.dirigo.rules :as rules]
            [clojure.core.memoize :as memo]
            [ring.adapter.jetty :refer [run-jetty]])
  (:import [java.net URI]))

(defn request->url
  [{:keys [scheme server-name uri query-string method]}]
  {:scheme scheme
   :host   server-name
   :path   uri
   :query  query-string
   :method method})

(defonce default-headers
  (atom
    {"Server"                    "Dirigo"
     "Permissions-Policy"        "interest-cohort=()"
     "Strict-Transport-Security" "max-age=31536000; includeSubDomains"
     "Content-Security-Policy"   "default-src 'self'"
     "X-XSS-Protection"          "1; mode=block"}))

(defn url->response
  [{:keys [type scheme user host port path query fragment]}]
  (if (and scheme host)
    {:status (case type
               :permanent 301
               :temporary 302
               301)
     :headers (assoc @default-headers
                     "Location"
                     (.toString
                       (URI. (name scheme) user host (or port -1) path query fragment)))}
    {:status  421
     :headers @default-headers}))

(defn apply-rule [ruleset url]
  (when-let [rule (get ruleset (:host url))]
    (rule url)))

(defonce memoized-redirector
  (memo/lru
    (fn [ruleset url]
      (if-let [url (apply-rule ruleset url)]
        (recur ruleset url)
        (url->response url)))
    {}
    :lru/threshold 32))

(defn redirector [request]
  (memoized-redirector
    @rules/ruleset
    (request->url request)))

(defn run
  [{:keys [port block?]
    :or   {port   8080
           block? true}}]
  (run-jetty
    #'redirector
    {:port  port
     :join? (not block?)
     :send-server-version? false}))
