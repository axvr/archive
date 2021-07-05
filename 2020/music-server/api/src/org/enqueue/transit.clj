(ns org.enqueue.transit
  "Wrapper around Transit to make it easier to use within Enqueue."
  (:require [cognitect.transit :as transit])
  (:import  [java.io ByteArrayInputStream ByteArrayOutputStream]))


;; TODO: headers
;; Accept: application/transit+json
;; Content-Type: application/transit+json


(def ^:private date-time-format
  (com.cognitect.transit.impl.AbstractParser/getDateTimeFormat))


;; Transit writer to convert java.time.Instant types to Transit dates.
(def ^:private instant-writer
  {java.time.Instant
   (transit/write-handler
     (constantly "t")
     (fn [^java.time.Instant inst]
       (.format
         date-time-format
         (java.util.Date/from inst))))})


;; Transit reader to convert Transit dates to java.time.Instants.
(def ^:private instant-reader
  {"t" (transit/read-handler #(.toInstant (.parse date-time-format %)))
   "m" (transit/read-handler #(java.time.Instant/ofEpochMilli (Long/parseLong %)))})


(defn encode
  "Encode data with Transit in JSON.  Returns data encoded as a string."
  [data]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json {:handlers instant-writer})]
    (transit/write writer data)
    (.toString stream)))


(defmulti decode
  "Decode Transit+JSON encoded data.  Returns decoded data."
  (fn [in & _] (type in)))

(defmethod decode ByteArrayInputStream
  [stream]
  (let [r (transit/reader stream :json {:handlers instant-reader})]
    (transit/read r)))

(defmethod decode ByteArrayOutputStream
  [stream]
  (decode (ByteArrayInputStream. (.toByteArray stream))))

(defmethod decode java.lang.String
  ([string]
   (decode string "UTF-8"))
  ([string charset]
   (decode (ByteArrayInputStream. (.getBytes string charset)))))
