(ns org.enqueue.router
  (:require
    [clout.core     :refer [route-matches route-compile]]
    [hiccup.core    :refer [html]]
    [clojure.string :refer [ends-with?]]))

(defn not-found-handler [request]
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body "<h1>404</h1>"})

(defn home-handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:html
                [:head
                 [:title "Enqueue API"]]
                [:body
                 [:h1 "Enqueue API"]
                 [:p "YOUR digital music collection, anywhere."]]])})

(defn about-handler [req] {:status 200
                           :headers {"Content-Type" "text/html"}
                           :body "<h1>About Enqueue</h1>"})

;; TODO: Rewrite/redirect rules.
(def route-map
  [["/"      {:get {:handler home-handler}}]
   ["/about" {:get {:handler about-handler}}]
   ["*"      {:all {:handler not-found-handler}}]])

;; (def route-map
;;   [["/"      {:get {:handler home-handler
;;                     :middleware [authorise]}]
;;    ["/admin" {:get {:handler home-handler
;;                     :middleware [(auth-policy :admin)]}]])

;; TODO: use middleware.
(defn- invoke-handler
  ([{:keys [handler middleware]} request]
   (handler request))
  ([{:keys [handler middleware]} request respond raise]
   (handler request respond raise)))

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

(defn wrap-ignore-trailing-slash [handler]
  (letfn [(fix-uri [request]
            (let* [path   (:uri request)
                   length (count path)]
              (if (and (not (= length 1))
                       (ends-with? path "/"))
                (let [trimed-path (subs path 0 (- length 1))]
                  (assoc request :uri trimed-path))
                request)))]
    (fn
      ([request]
       (handler (fix-uri request)))
      ([request respond raise]
       (handler (fix-uri request) respond raise)))))
