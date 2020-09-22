(ns org.enqueue.router
  (:require
    [clout.core                    :refer [route-matches]]
    [org.enqueue.router.middleware :refer [wrap-async]]
    [org.enqueue.handlers          :refer [home-handler
                                           about-handler
                                           not-found-handler]]))

;; TODO: Rewrite/redirect rules.

(def route-map
  [["/"      {:get {:handler home-handler
                    :middleware [wrap-async]}}]
   ["/about" {:get {:handler about-handler}}]
   ["*"      {:all {:handler not-found-handler}}]])

(defn- invoke-handler
  ([{:keys [handler middleware]} request]
   ;; TODO: make this a macro.
   ((eval (conj (seq middleware) handler '->)) request))
  ([{:keys [handler middleware]} request respond raise]
   ((eval (conj (seq middleware) handler '->)) request respond raise)))

(defn- route->handler [request route-map]
  (loop [routes route-map]
    (when (seq routes)
      (let* [method     (:request-method request)
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

(defn router [route-map]
  (fn
    ([req]
     (let [{:keys [handler request]}
           (route->handler req route-map)]
       (invoke-handler handler request)))
    ([req respond raise]
     (if-let [{:keys [handler request]}
              (route->handler req route-map)]
       (invoke-handler handler request respond raise)
       (raise (Throwable. (str "No HTTP handler defined for path " (:uri req))))))))
