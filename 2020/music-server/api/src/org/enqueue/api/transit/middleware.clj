(ns org.enqueue.api.transit.middleware
  "Middleware for automatically decode Transit request body and encoding
  response body.

  Partially based on Ring-JSON: <https://github.com/ring-clojure/ring-json>"
  (:require [org.enqueue.api.transit :as transit]
            [clojure.string :as str]))


(def content-type "application/transit+json")
(def content-type+charset (str content-type "; charset=UTF-8"))


(defn- get-header [headers header]
  (let [header (.toLowerCase header)]
    (some (fn [kv]
            (when (= (.toLowerCase (first kv)) header)
              kv))
          headers)))


(defn- transit-content-type? [ct]
  (str/starts-with? (.toLowerCase ct) content-type))


(defn- decode-body [{:keys [body] :as request}]
  (try
    (let [content-type (:content-type request)
          charset      (:character-encoding request "UTF-8")]
      (if (and body (transit-content-type? content-type))
        (assoc request :body (transit/decode body charset))
        request))
    (catch Exception _)))


(defn- encode-body [{:keys [body] :as response}]
  (let [ct-header      (get-header (:headers response) "Content-Type")
        ct-header-name (first ct-header)
        content-type   (or (second ct-header) content-type+charset)]
    (if (and body (transit-content-type? content-type))
      (-> (assoc response :body (transit/encode body))
          (assoc-in [:headers ct-header-name] content-type))
      response)))


(def default-malformed-data-response
  "Default response returned when invalid Transit data is sent in request body."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Malformed Transit data in request body"})


(defn wrap-transit-in
  "Middleware to automatically decode the body of a Transit request.  Will only
  decode when Content-Type is 'application/transit+json'.

  Accepts a map of the following options:
    :malformed-data-response -- Response used when malformed Transit data is in
                                the request body."
  ([handler]
   (wrap-transit-in handler {}))
  ([handler {:keys [malformed-data-response]
             :or   {malformed-data-response default-malformed-data-response}}]
   (fn
     ([request]
      (if-let [decoded-request (decode-body request)]
        (handler decoded-request)
        malformed-data-response))
     ([request respond raise]
      (if-let [decoded-request (decode-body request)]
        (handler decoded-request respond raise)
        (respond malformed-data-response))))))


(defn wrap-transit-out
  "Middleware to automatically encode the body of a Transit response.

  Will only encode when Content-Type header is missing or set to
  'application/transit+json'.  If Content-Type header is missing it will
  automatically be set."
  [handler]
  (fn
    ([request]
     (encode-body (handler request)))
    ([request respond raise]
     (handler request
              #(respond (encode-body %))
              raise))))


(defn wrap-transit
  "Middleware to automatically decode the body of a Transit request and encode
  the body of a Transit response.

  This middleware is a combination of 'wrap-transit-in' and 'wrap-transit-out'.

  Accepts a map of the following options:
    :malformed-data-response -- Response used when malformed Transit data is in
                                the request body."
  ([handler]
   (wrap-transit handler {}))
  ([handler options]
   (wrap-transit-out
     (wrap-transit-in handler options))))
