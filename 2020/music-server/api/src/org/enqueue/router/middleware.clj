(ns org.enqueue.router.middleware
  (:require [clojure.string :as string]
            [ring.util.response :refer [header]]
            [org.enqueue.helpers :refer [trim-end]]))


(defn- remove-trailing-slash [request]
  (let [path (:uri request)]
    (if (= path "/")
      request
      (assoc request :uri (trim-end path "/")))))


(defn wrap-ignore-trailing-slash
  "Ignore a trailing slash at the end of URI paths."
  [handler]
  (fn
    ([request]
     (handler (remove-trailing-slash request)))
    ([request respond raise]
     (handler (remove-trailing-slash request) respond raise))))


(defn wrap-async
  "Make a synchronous Ring handler asynchronous."
  [handler]
  (fn
    ([request]
     (handler request))
    ([request respond raise]
     (try
       (respond (handler request))
       (catch Exception e
         (raise e))))))


(defn- add-security-headers [resp origins]
  (-> resp
      (header "X-Frame-Options" "deny")
      (header "Content-Security-Policy"
              (str "default-src 'self' " (string/join " " origins)))
      (header "Permissions-Policy" "interest-cohort=()")
      (header "X-Content-Type-Options" "nosniff")
      ;; TODO: <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/strict-transport-security>
      #_(header "Strict-Transport-Security" "max-age=63072000; includeSubDomains; preload")))


(defn wrap-security-headers
  "Add various security and privacy headers to an HTTP response."
  [handler origins]
  (fn
    ([request]
     (add-security-headers (handler request) origins))
    ([request respond raise]
     (handler request
              (fn [response] (respond (add-security-headers response origins)))
              raise))))
