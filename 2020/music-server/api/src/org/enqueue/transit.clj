(ns org.enqueue.transit
  "Wrapper around Transit to make it easier to use within Enqueue."
  (:require [cognitect.transit :as transit])
  (:import  [java.io ByteArrayInputStream ByteArrayOutputStream]))


;; TODO: headers
;; Accept: application/transit+json
;; Content-Type: application/transit+json


(defn encode [data]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json)]
    (transit/write writer data)
    (.toString stream)))


(defmulti decode (fn [in & _] (type in)))

(defmethod decode ByteArrayInputStream
  [stream]
  (let [r (transit/reader stream :json)]
    (transit/read r)))

(defmethod decode ByteArrayOutputStream
  [stream]
  (decode (ByteArrayInputStream. (.toByteArray stream))))

(defmethod decode java.lang.String
  ([string]
   (decode string "UTF-8"))
  ([string charset]
   (decode (ByteArrayInputStream. (.getBytes string charset)))))
