(ns org.enqueue.core
  (:require
    [ring.adapter.jetty               :refer [run-jetty]]
    [ring.middleware.reload           :refer [wrap-reload]]
    [ring.middleware.params           :refer [wrap-params]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]
    [ring.middleware.resource         :refer [wrap-resource]]
    [ring.middleware.content-type     :refer [wrap-content-type]]
    [ring.middleware.not-modified     :refer [wrap-not-modified]]
    [org.enqueue.router               :refer [router fallback-routes]]
    [org.enqueue.router.middleware    :refer [wrap-ignore-trailing-slash
                                              wrap-security-headers
                                              wrap-async]]
    [org.enqueue.handlers             :refer [home-handler]]))

(def route-map
  (concat
    [["/" {:get {:handler home-handler
                 :middleware [wrap-async]}}]]
    fallback-routes))

(def cors-origins #{"https://www.enqueue.org"
                    "https://enqueue.org"
                    "https://api.enqueue.org"})

(def xss-origins #{"enqueue.org" "*.enqueue.org"})

(def app-handler
  (-> route-map
      (router cors-origins)
      (wrap-security-headers xss-origins)
      (wrap-resource "public")
      wrap-content-type
      wrap-not-modified
      wrap-params
      wrap-multipart-params
      wrap-ignore-trailing-slash))

(defn run [{:keys [dev? port]}]
  ;; TODO: other Jetty options (doc run-jetty).
  (run-jetty
    (if dev?
      (wrap-reload #'app-handler)
      #'app-handler)
    {:port port
     :async? true
     :send-server-version? false}))
