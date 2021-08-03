(ns org.enqueue.api.clients.middleware
  (:require [org.enqueue.api.clients.eat :as eat]
            [org.enqueue.api.config      :as config]))


(defn wrap-auth
  "Wrap handler with token extraction.  If token is invalid or missing,
  responds with HTTP status 401."
  [handler]
  (fn
    ([request]
     (if-let [token (eat/extract-token request config/signing-key)]
       (handler (assoc request :token token))
       {:status 401}))
    ([request respond raise]
     (if-let [token (eat/extract-token request config/signing-key)]
       (handler (assoc request :token token) respond raise)
       (respond {:status 401})))))
