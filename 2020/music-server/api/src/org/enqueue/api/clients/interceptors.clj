(ns org.enqueue.api.clients.interceptors
  (:require [org.enqueue.api.clients.eat   :as eat]
            [org.enqueue.api.config        :as config]
            [io.pedestal.interceptor       :as interceptor]
            [io.pedestal.interceptor.chain :as chain]))


(def ^:private default-unauthorised-response
  "Default response for when the provided authentication token is invalid."
  {:status  401
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Invalid EAT token"})


(defn eat-auth-interceptor
  "Add EAT token to context map.

  Accepts an optional map of the following keys:

    :unauthorised-response - Response returned when token is invalid or
                             missing."
  ([]
   (eat-auth-interceptor {}))
  ([{:keys [unauthorised-response]
     :or   {unauthorised-response default-unauthorised-response}}]
   (interceptor/interceptor
     {:name :eat-auth-interceptor
      :enter
      (fn [context]
        (if-let [token (eat/extract-token (:request context) config/signing-key)]
          (assoc-in context [:request :token] token)
          (chain/terminate
            (assoc context :response unauthorised-response))))})))
