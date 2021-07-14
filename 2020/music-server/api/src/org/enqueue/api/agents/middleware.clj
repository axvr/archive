(ns org.enqueue.api.agents.middleware
  (:require [org.enqueue.api.agents.eat :as eat]
            [org.enqueue.api.settings :as settings]))


(defn wrap-auth
  "Wrap handler with token extraction.  If token is invalid or missing,
  responds with HTTP status 401."
  [handler]
  (fn
    ([request]
     (if-let [token (eat/extract-token request settings/signing-key)]
       (handler (assoc request :token token))
       {:status 401}))
    ([request respond raise]
     (if-let [token (eat/extract-token request settings/signing-key)]
       (handler (assoc request :token token) respond raise)
       (respond {:status 401})))))
