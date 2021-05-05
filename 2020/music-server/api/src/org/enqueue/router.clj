(ns org.enqueue.router
  (:require [clout.core :refer [route-matches]]
            [org.enqueue.helpers :refer [splat-apply]]))

(defn- invoke
  ([{:keys [handler middleware]} request]
   ((splat-apply `-> handler middleware) request))
  ([{:keys [handler middleware]} request respond raise]
   ((splat-apply `-> handler middleware) request respond raise)))

(defn- route->handler [request route-map]
  (loop [routes route-map]
    (when (seq routes)
      (let [method     (:request-method request)
            route      (first routes)
            path       (first route)
            method-map (second route)
            handler    (get method-map method (:all method-map))]
        (if (some? handler)
          (let [matches (route-matches path request)]
            (if (some? matches)
              {:request (assoc request :uri-params matches)
               :handler handler}
              (recur (rest routes))))
          (recur (rest routes)))))))

(defn- no-handler-throwable [request]
  (Throwable.
    (str "No HTTP handler defined for path "
         (:uri request)
         " and method "
         (:request-method request))))

(defn router [route-map]
  (fn
    ([req]
     (let [{:keys [handler request]}
           (route->handler req route-map)]
       (if (some? (:handler handler))
         (invoke handler request)
         (throw (no-handler-throwable req)))))
    ([req respond raise]
     (let [{:keys [handler request]}
           (route->handler req route-map)]
       (if (some? (:handler handler))
         (invoke handler request respond raise)
         (raise (no-handler-throwable req)))))))
