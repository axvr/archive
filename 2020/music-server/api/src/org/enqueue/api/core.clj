(ns org.enqueue.api.core
  (:require
    [ring.adapter.jetty                :refer [run-jetty]]
    [ring.middleware.params            :refer [wrap-params]]
    [ring.middleware.keyword-params    :refer [wrap-keyword-params]]
    [ring.middleware.multipart-params  :refer [wrap-multipart-params]]
    [ring.middleware.resource          :refer [wrap-resource]]
    [ring.middleware.content-type      :refer [wrap-content-type]]
    [ring.middleware.not-modified      :refer [wrap-not-modified]]
    [org.enqueue.api.router            :refer [router]]
    [org.enqueue.api.router.middleware :refer [wrap-ignore-trailing-slash
                                               wrap-security-headers]]
    [org.enqueue.api.users             :refer [user-routes]]
    [org.enqueue.api.clients           :refer [client-routes]]
    [org.enqueue.api.docs              :refer [doc-routes]]
    [org.enqueue.api.config            :as    config]))


(defn- home-handler
  ([_]
   {:status 301
    :headers {"Location" "/docs"}})
  ([request respond _]
   (respond (home-handler request))))


(defn- not-found-handler
  ([_]
   {:status  404
    :headers {"Content-Type" "text/plain; charset=UTF-8"}
    :body    "404"})
  ([request respond _]
   (respond (not-found-handler request))))


(def fallback-routes
  [["*" {:get {:handler not-found-handler
               :middleware [wrap-content-type
                            wrap-not-modified
                            #(wrap-resource % "public")]}
         :all {:handler not-found-handler}}]])


(defn- build-route-map []
  (concat
    [["/" {:get {:handler home-handler}}]]
    user-routes
    client-routes
    doc-routes
    fallback-routes))


(def app-handler
  (-> build-route-map
      (router (get-in config/server [:origins :cors]) not-found-handler)
      (wrap-security-headers (get-in config/server [:origins :xss]))
      wrap-params
      wrap-keyword-params
      wrap-multipart-params
      wrap-ignore-trailing-slash))


(defn run [{:keys [port]
            :or {port (:port config/server)}}]
  (when config/prod?
    (set! *assert* false))  ; Disable assertions on production environment.
  ;; TODO: configure SSL, HSTS header + automatic redirect.
  (run-jetty
    #'app-handler
    {:port   port
     :async? (:async? config/server false)
     :send-server-version? false}))
