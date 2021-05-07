(ns org.enqueue.router
  "Custom Enqueue routing library for Ring."
  (:require [clout.core :refer [route-matches]]
            [org.enqueue.helpers :refer [while-nil when-let*]]
            [clojure.string :as string]))


(defn- apply-middleware
  "Apply middleware to a handler."
  [{:keys [handler middleware]}]
  (eval (filter some? (flatten (list '-> handler middleware)))))


(defn- get-handler [request route-map]
  (let [method (:request-method request)]
    (while-nil [[path method-map] route-map]
      (when-let* [handler (get method-map method (:all method-map))
                  matches (route-matches path request)]
       {:handler (apply-middleware handler)
        :request (assoc request :uri-params matches)}))))


(defn- no-handler-throwable [request]
  (Throwable.
    (str "No HTTP handler defined for path "
         (:uri request)
         " and method "
         (:request-method request))))


(defn- not-found-handler
  ([request]
   {:status 404
    :headers {"Content-Type" "text/html"}
    :body "<h1>404</h1>"})
  ([request respond raise]
   (respond (not-found-handler request))))


(def fallback-routes
  [["*" {:all {:handler not-found-handler}}]])


(defn- get-origin [request]
  (get-in request [:headers "origin"]))


(defn- add-origin [response request allowed-origins]
  (assoc-in
    response
    [:headers "Access-Control-Allow-Origin"]
    (let [browser-origin (get-origin request)]
      (if (allowed-origins browser-origin)
        browser-origin
        (first allowed-origins)))))


(defn- find-methods-for-path [request route-map]
  (while-nil [[path method-map] route-map]
    (when (route-matches path request)
      (filter #(not (= :all %))
              (keys method-map)))))


(defn- cors-preflight [request route-map allowed-origins]
  (let [methods (find-methods-for-path request route-map)]
    (add-origin
      (if (seq methods)
        (let [allowed-methods (->> (conj methods :options)
                                   (map name)
                                   (map string/upper-case)
                                   (string/join ", "))]
          {:status 204
           :headers {"Access-Control-Allow-Methods" allowed-methods
                     "Access-Control-Allow-Headers" "X-PINGOTHER, Content-Type"
                     "Access-Control-Max-Age" "86400"}})
        (not-found-handler request))
      request allowed-origins)))


(defn- cors-failed-handler
  ([allowed-origins request]
   (add-origin {:status 403} request allowed-origins))
  ([allowed-origins request respond raise]
   (respond (cors-failed-handler allowed-origins request))))


(defn- invoke-with-cors
  ([{:keys [handler request origins]}]
   (let [browser-origin (get-origin request)]
     (cond
       ;; No Origin header sent.
       (nil? browser-origin)
       (handler request)
       ;; Origin header sent and is allowed.
       (origins browser-origin)
       (add-origin (handler request) request origins)
       ;; Origin header sent but not allowed.
       :else
       (cors-failed-handler origins request))))
  ([{:keys [handler request origins]} respond raise]
   (println (get-origin request))
   (let [browser-origin (get-origin request)]
     (cond
       ;; No Origin header sent.
       (nil? browser-origin)
       (handler request respond raise)
       ;; Origin header sent and is allowed.
       (origins browser-origin)
       (handler request
                (fn [response] (respond (add-origin response request origins)))
                raise)
       ;; Origin header sent but not allowed.
       :else
       (cors-failed-handler origins request respond raise)))))


;; TODO: make CORS optional?  Disable when run locally.
(defn router [route-map-builder allowed-origins]
  (fn
    ([request]
     (let [route-map (route-map-builder)]
       (if (= :options (:request-method request))
         (cors-preflight request route-map allowed-origins)
         (let [handler (get-handler request route-map)]
           (if (some? (:handler handler))
             (invoke-with-cors (assoc handler :origins allowed-origins))
             (throw (no-handler-throwable
                      (get handler :request request))))))))
    ([request respond raise]
     (let [route-map (route-map-builder)]
       (if (= :options (:request-method request))
         (respond (cors-preflight request route-map allowed-origins))
         (let [handler (get-handler request route-map)]
           (if (some? (:handler handler))
             (invoke-with-cors (assoc handler :origins allowed-origins) respond raise)
             (raise (no-handler-throwable
                      (get handler :request request))))))))))
