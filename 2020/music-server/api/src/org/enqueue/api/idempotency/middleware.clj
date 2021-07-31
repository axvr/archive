(ns org.enqueue.api.idempotency.middleware
  "Idepotency layer of the Enqueue API.

  Inspired by Stripe's implementation [1] and a blog post by Ilija Eftimov [2].
  More information can be found in the doc-strings.

  [1]: https://stripe.com/docs/api/idempotent_requests
  [2]: https://ieftimov.com/post/understand-how-why-add-idempotent-requests-api/"
  (:require [clojure.core.cache.wrapped :as cache])
  (:import java.time.Duration java.util.UUID))


(defonce
  ^{:doc "Cache to store responses for idempotent requests.  TTL 6 hours."
    :private true}
  idempotent-cache
  (cache/ttl-cache-factory {} :ttl (.toMillis (Duration/ofHours 6))))


(def ^:private default-invalid-idempotency-key-response
  "Default response for when provided idempotency key is invalid."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Invalid idempotency key.  Must be a valid UUID."})


(def ^:private default-idempotency-key-recycled-response
  "Default response returned when an idempotency key was already used and the
  input request doesn't match the original request."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Possible idempotency key recycling detected"})


(defn wrap-idempotency-key
  "Adds an :idempotency-key item to the request map, if a valid idempotency key
  was given through the 'Idempotency-Key' header.

  Idempotency key must be a UUID, otherwise it will return an error.

  Accepts a map of the following (optional) options:
    :invalid-idempotency-key-response - Response to return when entered
                                        idempotency key is invalid."
  ([handler]
   (wrap-idempotency-key handler {}))
  ([handler {:keys [invalid-idempotency-key-response]
             :or   {invalid-idempotency-key-response
                    default-invalid-idempotency-key-response}}]
   (fn
     ([request]
      (if-let [idempotency-key (get-in request [:headers "idempotency-key"])]
        (try
          (handler
            (assoc request :idempotency-key (UUID/fromString idempotency-key)))
          (catch IllegalArgumentException _
            invalid-idempotency-key-response))
        (handler request)))
     ([request respond raise]
      (if-let [idempotency-key (get-in request [:headers "idempotency-key"])]
        (try
          (handler
            (assoc request :idempotency-key (UUID/fromString idempotency-key))
            respond
            raise)
          (catch IllegalArgumentException _
            (respond invalid-idempotency-key-response)))
        (handler request respond raise))))))


(defn- successful-response?
  "Returns true if the HTTP status of a response map is in the 200 range."
  [response]
  (<= 200 (:status response 200) 299))


(defn- build-idempotent-request-key
  "Build map used to heuristically detect accidental idempotency key recycles."
  [request]
  (let [{:keys [agent-id user-id]} (:token request)]
    (-> request
        (select-keys
          [:uri :request-method :query-string :content-type :content-length])
        (assoc :agent-id agent-id
               :user-id  user-id))))


(defn- wrap-idempotent-sync
  "Logic used by 'wrap-idempotent' for synchronous requests."
  [handler request idempotency-key idempotency-key-recycled-response]
  (let [req-key (build-idempotent-request-key request)
        result  (cache/lookup-or-miss
                  idempotent-cache
                  idempotency-key
                  (fn [resp-fn _]
                    {:request  req-key
                     :response (resp-fn)})
                  #(handler request))]
    (when-not (successful-response? (:response result))
      ;; If request was unsuccessful, evict it from the cache.
      (cache/evict idempotent-cache idempotency-key))
    (if (= (:request result) req-key)
      (:response result)
      idempotency-key-recycled-response)))


(defn- wrap-idempotent-async
  "Logic used by 'wrap-idempotent' for asynchronous requests."
  [handler request respond raise idempotency-key idempotency-key-recycled-response]
  (let [req-key (build-idempotent-request-key request)
        result  (cache/lookup idempotent-cache idempotency-key)]
    (if result
      (if (= (:request result) req-key)
        (respond (:response result))
        (respond idempotency-key-recycled-response))
      (handler request
               (fn [response]
                 (when (successful-response? response)
                   ;; This shouldn't be at risk of a cache stampede as
                   ;; idempotency keys should be unique.
                   (cache/miss idempotent-cache
                               idempotency-key
                               {:request  req-key
                                :response response}))
                 (respond response))
               raise))))


(defn wrap-idempotent
  "Wrap request in an idempotency layer.  Activates when an idempotency key is
  passed through the 'Idempotency-Key' header and is a UUID.

  Can only be used on POST requests (will error on any others), as all other
  request methods are idempotent by definition.

  If a request is made using an already cached idempotency key, this middleware
  will respond with an error if the request is not the same.  If this happens
  the client probably recycled (or forgot/failed to change) the key.

  Cache TTL is 6 hours.

  Only responses in the HTTP status 200 range are cached.

  Automatically wraps handler with 'wrap-idempotency-key' middleware.

  Accepts map of following (optional) options:
    :idempotency-key-recycled-response - Returned when an unintentional
                                         idempotency key reuse is detected."
  ([handler]
   (wrap-idempotent handler {}))
  ([handler {:keys [idempotency-key-recycled-response]
             :or   {idempotency-key-recycled-response
                    default-idempotency-key-recycled-response}}]
   (fn
     ([request]
      (assert (= (:request-method request) :post)
              "Idempotency layer can only be used on POST requests.")
      (if-let [idempotency-key (:idempotency-key request)]
        (wrap-idempotent-sync
          (wrap-idempotency-key handler)
          request
          idempotency-key
          idempotency-key-recycled-response)
        (handler request)))
     ([request respond raise]
      (assert (= (:request-method request) :post)
              "Idempotency layer can only be used on POST requests.")
      (if-let [idempotency-key (:idempotency-key request)]
        (wrap-idempotent-async
          (wrap-idempotency-key handler)
          request
          respond
          raise
          idempotency-key
          idempotency-key-recycled-response)
        (handler request respond raise))))))
