(ns uk.axvr.dirigo.server
  (:require [uk.axvr.dirigo.rules :as rules]
            [uk.axvr.dirigo.acme :as acme]
            [clojure.core.memoize :as memo]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.string :as str])
  (:import [java.net URI]))

(defn request->url
  [{:keys [scheme server-name uri query-string method]}]
  {:scheme scheme
   :host   server-name
   :path   uri
   :query  query-string
   :method method})

(def ^:private default-headers
  {"Server"                    "Dirigo"
   "Permissions-Policy"        "interest-cohort=()"
   "Strict-Transport-Security" "max-age=31536000; includeSubDomains"
   "Content-Security-Policy"   "default-src 'self'"
   "X-XSS-Protection"          "1; mode=block"})

(defn url->response
  [{:keys [type scheme user host port path query fragment]}]
  (if (and scheme host)
    {:status (case type
               :permanent 301
               :temporary 302
               301)
     :headers (assoc default-headers
                     "Location"
                     (.toString
                       (URI. (name scheme) user host (or port -1) path query fragment)))}
    {:status  421
     :headers default-headers}))

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
    :lru/threshold 512))

(defn redirector [request respond _raise]
  (respond
   (let [url (request->url request)]
     (if (str/starts-with? (:path url) "/.well-known/acme-challenge/")
       (acme/http01-challenge-handler request)
       (memoized-redirector @rules/ruleset url)))))

(defn run
  [{:keys [port block?]
    :or   {port   8080
           block? true}}]
  (run-jetty
    #'redirector
    {:port port
     :join? (not block?)
     :async? true
     :send-server-version? false}))

;; Domain names that need TLS.
;; Host(`axvr.uk`, `www.axvr.uk`, `git.axvr.uk`, `ascribe.axvr.uk`, `alexvear.com`)
;; Host(`axvr.io`, `www.axvr.io`, `ascribe.axvr.io`)
;; Host(`***REMOVED***.com`, `www.***REMOVED***.com`)
