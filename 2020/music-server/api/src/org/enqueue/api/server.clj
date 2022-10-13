(ns org.enqueue.api.server
  (:require [clojure.set             :as set]
            [clojure.string          :as str]
            [io.pedestal.http        :as http]
            [io.pedestal.http.route  :as route]
            [io.pedestal.interceptor :as interceptor]
            [org.enqueue.api.utils   :as u]
            [org.enqueue.api.clients :as clients]
            [org.enqueue.api.config  :as config]
            [org.enqueue.api.docs    :as docs]
            [org.enqueue.api.users   :as users]
            [org.enqueue.api.transit.interceptors :refer [transit-in-interceptor]])
  (:import [org.eclipse.jetty.server.handler.gzip GzipHandler]))

(def root
  {:name ::index
   :enter
   #(assoc % :response {:status 301
                        :headers {"Location" "/docs"}})})

(defn build-routes []
  (route/expand-routes
    (set/union
     #{["/" :get root]}
     users/routes
     clients/routes
     docs/routes)))

(if config/prod?
  (do
    (defonce route-atom (atom (build-routes)))
    (defn routes [] @route-atom))
  (defn routes [] (build-routes)))

(def privacy-headers
  (interceptor/interceptor
   {:name  ::privacy-headers
    :leave #(assoc-in % [:response :headers "Permissions-Policy"] "interest-cohort=()")}))

;; https://github.com/pedestal/pedestal/blob/master/samples/servlet-filters-gzip/src/gzip/service.clj
(defn- context-configurator
  [context]
  (let [gzip-handler (GzipHandler.)]
    (.setGzipHandler context gzip-handler)
    context))

(defonce server (atom nil))

(defn start!
  [{:keys [port join?]
    :or   {port  (:port config/server)
           join? (:join? config/server true)}}]
  (when @server
    (throw (ex-info "Server already running." @server))) 
  (swap! server
         (fn [_]
           ;; TODO: configure TLS, HSTS header + automatic redirect.
           ;; - <http://pedestal.io/reference/service-map>
           ;; - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/strict-transport-security>
           (let [s (-> {::http/routes routes
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
                              (str/join " " (get-in config/server [:origins :xss])))}
                        ::http/container-options {:context-configurator context-configurator}}
                       http/default-interceptors
                       (update ::http/interceptors conj
                               privacy-headers
                               (transit-in-interceptor))
                       http/create-server
                       http/start)]
             (u/on-shutdown! (when s (http/stop s)))
             s))))

(defn stop! []
  (swap! server #(and (http/stop %) nil)))
