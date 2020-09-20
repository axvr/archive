(ns org.enqueue.main
  (:require
    [ring.adapter.jetty               :refer [run-jetty]]
    [ring.middleware.reload           :refer [wrap-reload]]
    [ring.middleware.params           :refer [wrap-params]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]
    [org.enqueue.router               :refer [router route-map
                                              wrap-ignore-trailing-slash]]
    [ring.middleware.resource :refer [wrap-resource]]
    [ring.middleware.content-type :refer [wrap-content-type]]))

;; TODO: spec.
;; [clojure.spec.alpha :as s]

;; TODO: DB.
;; TODO: File upload/download and storage.
;; TODO: Authentication.
;; TODO: appsettings.

(def dev-handler
  (-> (router route-map)
      (wrap-resource "public")
      ;; wrap-content-type
      wrap-reload
      wrap-params
      wrap-multipart-params
      wrap-ignore-trailing-slash))

;; TODO: make app async.
;; TODO: accept map which specifies mode/env and port.
;; TODO: HTTPS redirection?
(defn -main [& args]
  (run-jetty #'dev-handler {:port 3000}))
