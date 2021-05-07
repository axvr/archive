(ns org.enqueue.router
  (:require [clout.core :refer [route-matches]]
            [org.enqueue.helpers :refer [while-nil]]
            [clojure.string :as string]))


(defn- apply-middleware [handler middleware]
  (eval (filter some? (flatten (list '-> handler middleware)))))


(defn- invoke
  ([{:keys [handler middleware request]}]
   ((apply-middleware handler middleware) request))
  ([{:keys [handler middleware request]} respond raise]
   ((apply-middleware handler middleware) request respond raise)))


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


(defn- find-methods-for-path [request route-map]
  (while-nil [[path method-map] route-map]
    (when (route-matches path request)
      (filter #(not (= :all %))
              (keys method-map)))))


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


(defn- get-cors-allowed-origin
  "If browser sent allowed origin return it, else return an allowed origin."
  [request origins]
  (let [browser-origin (get-in request [:headers "origin"])]
    (if (origins browser-origin)
      browser-origin
      (first origins))))


(defn- wrap-cors-preflight [request route-map origins]
  (when (= :options (:request-method request))
    (let [methods (find-methods-for-path request route-map)]
      (if (seq methods)
        (let [allowed-methods (->> (conj methods :options)
                                   (map name)
                                   (map string/upper-case)
                                   (string/join ", "))]
          {:status 204
           :headers {"Access-Control-Allow-Methods" allowed-methods
                     "Access-Control-Allow-Headers" "X-PINGOTHER, Content-Type"
                     "Access-Control-Allow-Origin" (get-cors-allowed-origin request origins)
                     "Access-Control-Max-Age" "86400"}})
        (assoc-in (not-found-handler request)
                  [:headers "Access-Control-Allow-Origin"] (get-cors-allowed-origin request origins))))))


(defn- cors-check-complex [request route-map origins]
  (if-let [origin (get-in request [:headers "origin"])]
    (if (origins origin) :allow :block)
    :no-cors))


;; TODO: experiment with per-endpoint CORS.
;; ^ per-section endpoint configs?


;; TODO: test CORS on a non-localhost domain/server.
;; TODO: make CORS optional?  Disable on dev?
;; TODO: eliminate duplicate code.
(defn router [route-map origins]
  (fn
    ([request]
     (if-let [preflight-resp (wrap-cors-preflight request route-map origins)]
       preflight-resp
       (let [cors-status (cors-check-complex request route-map origins)]
         (if (= cors-status :block)
           {:status 403
            :headers {"Access-Control-Allow-Origin" (get-cors-allowed-origin request origins)}}
           (let [handler (find-handler request route-map)]
             (if (some? (:handler handler))
               (if (= cors-status :allow)
                 (assoc-in (invoke handler)
                           [:headers "Access-Control-Allow-Origin"] (get-cors-allowed-origin request origins))
                 (invoke handler))
               (throw (no-handler-throwable
                        (get handler :request request)))))))))
    ([request respond raise]
     (if-let [preflight-resp (wrap-cors-preflight request route-map origins)]
       (respond preflight-resp)
       (let [cors-status (cors-check-complex request route-map origins)]
         (if (= cors-status :block)
           (respond {:status 403
                     :headers {"Access-Control-Allow-Origin" (get-cors-allowed-origin request origins)}})
           (let [handler (find-handler request route-map)]
             (if (some? (:handler handler))
               (invoke handler
                       (if (= cors-status :allow)
                         (fn [response]
                           (respond (assoc-in
                                      response
                                      [:headers "Access-Control-Allow-Origin"]
                                      (get-cors-allowed-origin request origins))))
                         respond)
                       raise)
               (raise (no-handler-throwable
                        (get handler :request request)))))))))))
