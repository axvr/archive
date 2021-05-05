(ns org.enqueue.router
  (:require [clout.core :refer [route-matches]]
            [org.enqueue.helpers :refer [splat-apply while-nil]]))


(defn- invoke
  ([{:keys [handler middleware request]}]
   ((splat-apply '-> handler middleware) request))
  ([{:keys [handler middleware request]} respond raise]
   ((splat-apply '-> handler middleware) request respond raise)))


(defn- find-handler [request route-map]
  (let [method (:request-method request)]
    (while-nil [route route-map]
      (let [[path method-map] route
            handler (get method-map method (:all method-map))]
        (when (some? handler)
          (when-let [matches (route-matches path request)]
            (assoc handler
                   :request
                   (assoc request :uri-params matches))))))))


(defn- no-handler-throwable [request]
  (Throwable.
    (str "No HTTP handler defined for path "
         (:uri request)
         " and method "
         (:request-method request))))


(defn router [route-map]
  (fn
    ([request]
     (let [handler (find-handler request route-map)]
       (if (some? (:handler handler))
         (invoke handler)
         (throw (no-handler-throwable
                  (get handler :request request))))))
    ([request respond raise]
     (let [handler (find-handler request route-map)]
       (if (some? (:handler handler))
         (invoke handler respond raise)
         (raise (no-handler-throwable
                  (get handler :request request))))))))
