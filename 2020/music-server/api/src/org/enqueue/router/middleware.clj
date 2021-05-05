(ns org.enqueue.router.middleware
  (:require [clojure.string :as string]
            [ring.util.response :refer [header]]))


(defn- remove-trailing-slash [request]
  (let [path   (:uri request)
        length (count path)]
    (if (and (not (= length 1))
             (string/ends-with? path "/"))
      (let [trimed-path (subs path 0 (- length 1))]
        (assoc request :uri trimed-path))
      request)))


(defn wrap-ignore-trailing-slash
  "Ignore a trailing slash at the end of URI paths."
  [handler]
  (fn
    ([request]
     (handler (remove-trailing-slash request)))
    ([request respond raise]
     (handler (remove-trailing-slash request) respond raise))))


(defn wrap-async
  "Make any synchronous Ring handler asynchronous."
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
      (header "Permissions-Policy" "interest-cohort=()")))


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
