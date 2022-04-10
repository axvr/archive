(ns org.enqueue.api.transit.interceptors
  "Interceptors to automatically decode Transit request body and encoding
  response body."
  (:require [org.enqueue.api.transit :as transit]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.interceptor.chain :as chain]
            [clojure.string :as str]))


(def content-type "application/transit+json")
(def content-type+charset (str content-type "; charset=UTF-8"))


(defn- get-header [headers header]
  (let [header (str/lower-case header)]
    (some (fn [kv]
            (when (= (str/lower-case (first kv)) header)
              kv))
          headers)))


(defn- media-type [content-type]
  (some-> content-type
          (str/split #"\s*;\s*" 2)
          first
          str/lower-case))


(defn- transit-content-type? [ct]
  (= (media-type ct) content-type))


(defn- decode-body [request]
  (let [body         (:body request)
        content-type (:content-type request)
        charset      (:character-encoding request "UTF-8")]
    (if (and (pos? (:content-length request 0))
             (transit-content-type? content-type))
      (assoc request :body (transit/decode body charset))
      request)))


(defn- encode-body [{:keys [body] :as response}]
  (let [ct-header      (get-header (:headers response) "Content-Type")
        ct-header-name (first ct-header)
        content-type   (or (second ct-header) content-type+charset)]
    (if (and body (transit-content-type? content-type))
      (-> (assoc response :body (transit/encode body))
          (assoc-in [:headers ct-header-name] content-type))
      response)))


(def ^:private default-malformed-data-response
  "Default response returned when invalid Transit data is sent in request body."
  {:status  400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "Malformed Transit data in request body"})


(defn transit-in-interceptor
  "Interceptor to automatically decode the body of a Transit request.  Will
  only decode when Content-Type is 'application/transit+json'.

  Accepts a map of the following options:
    :malformed-data-response -- Response used when malformed Transit data is in
                                the request body."
  ([]
   (transit-in-interceptor {}))
  ([{:keys [malformed-data-response]
     :or   {malformed-data-response default-malformed-data-response}}]
   (interceptor/interceptor
     {:name ::in
      :enter
      (fn [context]
        (try
          (update context :request decode-body)
          (catch Exception _
            (chain/terminate
              (assoc context :response malformed-data-response)))))})))


(def transit-out-interceptor
  "Interceptor to automatically encode the body of a Transit response.

  Will only encode when Content-Type header is missing or set to
  'application/transit+json'.  If Content-Type header is missing it will
  automatically be set."
  (interceptor/interceptor
    {:name ::out
     :leave #(update % :response encode-body)}))
