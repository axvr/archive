(ns org.enqueue.api.transit
  "Wrapper around Transit to make it easier to use within Enqueue."
  (:require [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [uk.axvr.refrain :as r])
  (:import  [java.io ByteArrayInputStream
                     ByteArrayOutputStream
                     InputStream
                     InputStreamReader
                     BufferedReader
                     File
                     FileInputStream]
            [java.time Instant Duration]))

(def ^:private ^:const default-charset "UTF-8")

(def ^:private date-time-format
  "Default date format used by Transit."
  (com.cognitect.transit.impl.AbstractParser/getDateTimeFormat))

(def ^:private inst-writer-handlers
  "Transit writer handlers to convert java.time.Instant types to Transit
  dates."
  {Instant
   (transit/write-handler
     (constantly "t")
     (fn [^Instant inst]
       (.format
         date-time-format
         (java.util.Date/from inst))))})

(def ^:private inst-reader-handlers
  "Transit reader handlers to convert Transit dates to java.time.Instants."
  {"t" (transit/read-handler #(.toInstant (.parse date-time-format %)))
   "m" (transit/read-handler #(Instant/ofEpochMilli (Long/parseLong %)))})

(def ^:private duration-writer-handler
  "Transit writer handler to convert java.time.Duration types to a custom
  Transit extension with tag 'dur' containing a number of nanoseconds."
  {Duration
   (transit/write-handler
     (constantly "dur")
     (fn [^Duration dur]
       (.. dur toNanos toString)))})

(def ^:private duration-reader-handler
  "Transit reader handler to convert custom Transit extension with tag 'dur'
  representing a number of nanoseconds to a java.time.Duration."
  {"dur"
   (transit/read-handler
     (fn [v]
       (let [nanos (Long/parseLong v)]
         (Duration/ofNanos nanos))))})

(def ^:private writer-handlers
  {:handlers (merge inst-writer-handlers duration-writer-handler)})

(def ^:private reader-handlers
  {:handlers (merge inst-reader-handlers duration-reader-handler)})

(defn encode
  "Encode data with Transit.  Returns encoded data as a string."
  ([data]
   (encode data {}))
  ([data opts]
   (with-open [stream (ByteArrayOutputStream.)]
     (let [opts   (r/deep-merge writer-handlers opts)
           writer (transit/writer stream (:format opts :json) opts)]
       (transit/write writer data)
       (str stream)))))

(defmulti decode
  "Decode Transit encoded data.  Returns decoded data."
  (fn [in & _] (class in)))

(defmethod decode ByteArrayInputStream
  ([^ByteArrayInputStream stream]
   (decode stream {}))
  ([^ByteArrayInputStream stream opts]
   (let [opts   (r/deep-merge reader-handlers opts)
         reader (transit/reader stream (:format opts :json) opts)]
     (transit/read reader))))

(defmethod decode ByteArrayOutputStream
  ([^ByteArrayOutputStream stream]
   (decode stream {}))
  ([^ByteArrayOutputStream stream opts]
   (decode (ByteArrayInputStream. (.toByteArray stream)) opts)))

(defmethod decode InputStream
  ([^InputStream stream]
   (decode stream {}))
  ([^InputStream stream opts]
   (with-open [charset (:charset opts default-charset)
               in      (InputStreamReader. stream charset)
               buf     (BufferedReader. in)
               out     (ByteArrayOutputStream.)]
     (io/copy buf out :encoding charset)
     (decode out opts))))

(defmethod decode File
  ([^File file]
   (decode file {}))
  ([^File file opts]
   (with-open [in-stream (FileInputStream. file)]
     (decode in-stream opts))))

(defmethod decode String
  ([^String string]
   (decode string {}))
  ([^String string opts]
   (when (seq string)
     (decode (ByteArrayInputStream.
               (.getBytes string (:charset opts default-charset)))
             opts))))

(defmethod decode nil [& _] nil)
