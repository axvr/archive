(ns org.enqueue.main
  (:require
    [ring.adapter.jetty               :refer [run-jetty]]
    [ring.middleware.reload           :refer [wrap-reload]]
    [ring.middleware.params           :refer [wrap-params]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]
    [ring.middleware.resource         :refer [wrap-resource]]
    [ring.middleware.content-type     :refer [wrap-content-type]]
    [ring.middleware.not-modified     :refer [wrap-not-modified]]
    [org.enqueue.router               :refer [router route-map]]
    [org.enqueue.router.middleware    :refer [wrap-ignore-trailing-slash]]))

;; TODO: Authentication.
;; TODO: Appsettings.
;; TODO: File upload/download and storage.

(def dev-handler
  (-> (router route-map)
      (wrap-resource "public")
      wrap-content-type
      wrap-not-modified
      wrap-reload
      wrap-params
      wrap-multipart-params
      wrap-ignore-trailing-slash))

;; TODO: accept map which specifies mode/env and port.
;; TODO: other Jetty options (doc run-jetty).
(defn -main [& args]
  (run-jetty #'dev-handler {:port 3000, :async? true}))
