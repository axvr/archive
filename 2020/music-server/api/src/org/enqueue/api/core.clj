(ns org.enqueue.api.core
  (:require
    [ring.adapter.jetty                :refer [run-jetty]]
    [ring.middleware.params            :refer [wrap-params]]
    [ring.middleware.multipart-params  :refer [wrap-multipart-params]]
    [ring.middleware.resource          :refer [wrap-resource]]
    [ring.middleware.content-type      :refer [wrap-content-type]]
    [ring.middleware.not-modified      :refer [wrap-not-modified]]
    [org.enqueue.api.router            :refer [router]]
    [org.enqueue.api.router.middleware :refer [wrap-ignore-trailing-slash
                                               wrap-security-headers
                                               wrap-async]]
    [org.enqueue.api.users             :refer [user-routes]]
    [org.enqueue.api.agents            :refer [agent-routes]]
    [org.enqueue.api.config            :as    config]))


(defn- home-handler [_]
  {:status 200
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body (str "<title>Enqueue API</title>"
              "<h1>Enqueue API</h1>"
              "<p>Your digital music collection, anywhere.</p>")})


(defn- not-found-handler
  ([_]
   {:status 404
    :headers {"Content-Type" "text/html; charset=UTF-8"}
    :body "<h1>404</h1>"})
  ([request respond _]
   (respond (not-found-handler request))))


(def fallback-routes
  [["*" {:get {:handler not-found-handler
               :middleware [#(wrap-resource % "public")
                            wrap-content-type
                            wrap-not-modified]}
         :all {:handler not-found-handler}}]])


(defn- build-route-map []
  (concat
    [["/" {:get {:handler home-handler
                 :middleware [wrap-async]}}]]
    user-routes
    agent-routes
    fallback-routes))


(def app-handler
  (-> build-route-map
      (router (get-in config/server [:origins :cors]) not-found-handler)
      (wrap-security-headers (get-in config/server [:origins :xss]))
      wrap-params
      wrap-multipart-params
      wrap-ignore-trailing-slash))


(defn run [{:keys [port]}]
  (when (= config/env :prod)
    ;; Disable assertions on production environment.
    (set! *assert* false))
  ;; TODO: other Jetty options (doc run-jetty).
  (run-jetty
    #'app-handler
    {:port (or port (:port config/server))
     :async? true
     :send-server-version? false}))
