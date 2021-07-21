(ns org.enqueue.api.transit
  "Wrapper around Transit to make it easier to use within Enqueue."
  (:require [cognitect.transit :as transit])
  (:import  [java.io ByteArrayInputStream
                     ByteArrayOutputStream
                     InputStream
                     InputStreamReader
                     BufferedReader]))


(def ^:private date-time-format
  "Default date format used by Transit."
  (com.cognitect.transit.impl.AbstractParser/getDateTimeFormat))


(def ^:private inst-writer-handlers
  "Transit writer handlers to convert java.time.Instant types to Transit
  dates."
  {java.time.Instant
   (transit/write-handler
     (constantly "t")
     (fn [^java.time.Instant inst]
       (.format
         date-time-format
         (java.util.Date/from inst))))})


(def ^:private inst-reader-handlers
  "Transit reader handlers to convert Transit dates to java.time.Instants."
  {"t" (transit/read-handler #(.toInstant (.parse date-time-format %)))
   "m" (transit/read-handler #(java.time.Instant/ofEpochMilli (Long/parseLong %)))})


(defn encode
  "Encode data with Transit in JSON.  Returns data encoded as a string."
  [data]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json {:handlers inst-writer-handlers})]
    (transit/write writer data)
    (.toString stream)))


(defmulti decode
  "Decode Transit+JSON encoded data.  Returns decoded data."
  (fn [in & _] (class in)))

(defmethod decode ByteArrayInputStream
  [^ByteArrayInputStream stream]
  (let [r (transit/reader stream :json {:handlers inst-reader-handlers})]
    (transit/read r)))

(defmethod decode ByteArrayOutputStream
  [^ByteArrayOutputStream stream]
  (decode (ByteArrayInputStream. (.toByteArray stream))))

(defmethod decode InputStream
  ([^InputStream stream]
   (decode stream "UTF-8"))
  ([^InputStream stream charset]
   (with-open [in-reader  (InputStreamReader. stream charset)
               buf-reader (BufferedReader. in-reader)
               out-stream (ByteArrayOutputStream.)]
     (loop [byte (.read buf-reader)]
       (when-not (= byte -1)
         (.write out-stream byte)
         (recur (.read buf-reader))))
     (decode out-stream))))

(defmethod decode java.lang.String
  ([^String string]
   (decode string "UTF-8"))
  ([^String string charset]
   (when (seq string)
     (decode (ByteArrayInputStream. (.getBytes string charset))))))

(defmethod decode nil [& _] nil)
