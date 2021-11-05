(ns org.enqueue.api.core
  (:require
    [clojure.set             :as set]
    [clojure.string          :as str]
    [io.pedestal.http        :as http]
    [io.pedestal.http.route  :as route]
    [io.pedestal.interceptor :as interceptor]
    [org.enqueue.api.config  :as config]
    [org.enqueue.api.docs    :refer [docs-routes]]
    [org.enqueue.api.users   :refer [user-routes]]
    [org.enqueue.api.clients :refer [client-routes]]
    [org.enqueue.api.transit.interceptors :refer [transit-in-interceptor]]))


(def root
  {:name :root-index
   :enter
   #(assoc % :response {:status 301
                        :headers {"Location" "/docs"}})})


(def privacy-headers
  (interceptor/interceptor
    {:name :privacy-headers
     :leave
     #(assoc-in % [:response :headers "Permissions-Policy"] "interest-cohort=()")}))


(defn build-routes []
  (route/expand-routes
    (set/union
      #{["/" :get root]}
      user-routes
      client-routes
      docs-routes)))


(if config/prod?
  (do
    (defonce route-atom (atom (build-routes)))
    (defn routes [] @route-atom))
  (defn routes [] (build-routes)))


(defonce server (atom nil))


(defn run
  ([]
   (run {}))
  ([{:keys [port join?]
     :or {port  (:port config/server)
          join? true}}]
   (when config/prod?
     (set! *assert* false))  ; Disable assertions on production environment.
   ;; TODO: configure SSL, HSTS header + automatic redirect.
   ;; - <http://pedestal.io/reference/service-map>
   ;; - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/strict-transport-security>
   (reset! server (-> {::http/routes routes
                       ::http/type   :jetty
                       ::http/join?  join?
                       ::http/port   port
                       ::http/resource-path "public"
                       ::http/allowed-origins (get-in config/server [:origins :cors])
                       ::http/secure-headers
                       {:hsts-settings "max-age=31536000; includeSubdomains"
                        :frame-options-settings "deny"
                        :content-type-settings "nosniff"
                        :xss-protection-settings "1; mode=block"
                        :download-options-settings "noopen"
                        :cross-domain-policies-settings "none"
                        :content-security-policy-settings
                        (str "default-src 'self' "
                             (str/join " " (get-in config/server [:origins :xss])))}}
                      http/default-interceptors
                      (update ::http/interceptors conj
                              privacy-headers
                              (transit-in-interceptor))
                      http/create-server))
   (http/start @server)))


(defn stop []
  (http/stop @server))
