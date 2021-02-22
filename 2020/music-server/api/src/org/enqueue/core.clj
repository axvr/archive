(ns org.enqueue.core
  (:require
    [ring.adapter.jetty               :refer [run-jetty]]
    [ring.middleware.reload           :refer [wrap-reload]]
    [ring.middleware.params           :refer [wrap-params]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]
    [ring.middleware.resource         :refer [wrap-resource]]
    [ring.middleware.content-type     :refer [wrap-content-type]]
    [ring.middleware.not-modified     :refer [wrap-not-modified]]
    [org.enqueue.router               :refer [router]]
    [org.enqueue.router.middleware    :refer [wrap-ignore-trailing-slash
                                              wrap-async]]
    [org.enqueue.handlers             :refer [home-handler
                                              about-handler
                                              not-found-handler]]))

(def route-map
  [["/"      {:get {:handler home-handler
                    :middleware [wrap-async]}}]
   ["/about" {:get {:handler about-handler
                    :middleware [wrap-async]}}]
   ["*"      {:all {:handler not-found-handler
                    :middleware [wrap-async]}}]])

(def app-handler
  (-> (router route-map)
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
    {:port port, :async? true}))
