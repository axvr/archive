(ns org.enqueue.api.idempotency.interceptors
  "Idempotency layer of the Enqueue API.

  Inspired by Stripe's implementation [1] and a blog post by Ilija Eftimov [2].
  More information can be found in the doc-strings.

  [1]: https://stripe.com/docs/api/idempotent_requests
  [2]: https://ieftimov.com/post/understand-how-why-add-idempotent-requests-api/"
  (:require [clojure.core.cache.wrapped    :as cache]
            [io.pedestal.interceptor       :as interceptor]
            [io.pedestal.interceptor.chain :as chain])
  (:import [java.time Duration]))

(defonce
  ^{:doc "Cache to store responses for idempotent requests.  TTL 6 hours."
    :private true}
  idempotent-cache
  (cache/ttl-cache-factory {} :ttl (.toMillis (Duration/ofHours 6))))

(def ^:private default-invalid-idempotency-key-response
  "Default response for when the provided idempotency key is invalid."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Invalid idempotency key.  Must be a valid UUID."})

(def ^:private default-idempotency-key-recycled-response
  "Default response returned when an idempotency key was already used and the
  request doesn't match the original request."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Possible idempotency key recycling detected"})

(defn- successful-response?
  "Returns true if the HTTP status of a response map is in the 200 range."
  [response]
  (<= 200 (:status response 200) 299))

(defn- build-request-id
  "Build map used to heuristically detect accidental idempotency key recycles."
  [request]
  (let [{:keys [client-id user-id]} (:token request)]
    (-> request
        (select-keys
          [:uri :request-method :query-string :content-type :content-length])
        (assoc :client-id client-id
               :user-id   user-id))))

(defn idempotency-interceptor
  "Wrap request in an idempotency layer.  Activates when an idempotency key is
  passed through the 'Idempotency-Key' header and is a UUID.

  Will only run for POST requests, as all other request methods are idempotent
  by definition.

  If a request is made using an already cached idempotency key, this middleware
  will respond with an error if the request is not the same.  If this happens
  the client probably recycled (or forgot/failed to change) the key.

  Cache TTL is 6 hours.

  Only responses in the HTTP status 200 range are cached.

  Note: this interceptor needs to be used after the auth interceptors.

  Accepts a map of following (optional) options:

    :idempotency-key-recycled-response - Returned when an unintentional
                                         idempotency key reuse is detected.

    :invalid-idempotency-key-response  - Returned when an invalid idempotency
                                         key was given."
  ([]
   (idempotency-interceptor {}))
  ([{:keys [idempotency-key-recycled-response
            invalid-idempotency-key-response]
     :or   {idempotency-key-recycled-response default-idempotency-key-recycled-response
            invalid-idempotency-key-response  default-invalid-idempotency-key-response}}]
   (interceptor/interceptor
     {:name ::idempotent
      :enter
      (fn [context]
        (if (= :post (get-in context [:request :request-method]))
          (if-let [idempotency-key (get-in context [:request :headers "idempotency-key"])]
            (try
              (let [idempotency-key (parse-uuid idempotency-key)
                    context (assoc-in context [:request :idempotency-key] idempotency-key)]
                (if-let [cached-resp (cache/lookup idempotent-cache idempotency-key)]
                  (chain/terminate
                    (assoc context :response
                           (let [request-id (build-request-id (:request context))]
                             (if (= (:request cached-resp) request-id)
                               cached-resp
                               idempotency-key-recycled-response))))
                  context))
              (catch IllegalArgumentException _
                (chain/terminate
                  (assoc context :response invalid-idempotency-key-response))))
            context)
          context))
      :leave
      (fn [context]
        (when-let [response (:response context)]
          (when-let [idempotency-key (get-in context [:request :idempotency-key])]
            (when (successful-response? response)
              ;; This shouldn't be at risk of a cache stampede as idempotency
              ;; keys should be unique.
              (cache/miss idempotent-cache
                          idempotency-key
                          {:request  (build-request-id (:request context))
                           :response response}))))
        context)})))
