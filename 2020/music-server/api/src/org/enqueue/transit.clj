(ns org.enqueue.transit
  "Wrapper around Transit to make it easier to use within Enqueue."
  (:require [cognitect.transit :as transit])
  (:import  [java.io ByteArrayInputStream ByteArrayOutputStream]))


;; TODO: headers
;; Accept: application/transit+json
;; Content-Type: application/transit+json


;; TODO: auto-string?
(defn writer
  ([data]
   (writer (ByteArrayOutputStream.) data))
  ([stream data]
   (let [writer (transit/writer stream :json)]
     (transit/write writer data)
     stream)))


(defmulti reader (fn [in & args] (type in)))

;; FIXME: cannot read multiple values.
(defmethod reader ByteArrayInputStream
  [stream]
  (let [r (transit/reader stream :json)]
    (transit/read r)))

(defmethod reader ByteArrayOutputStream
  [stream]
  (reader (ByteArrayInputStream. (.toByteArray stream))))

(defmethod reader java.lang.String
  ([string]
   (reader string "UTF-8"))
  ([string charset]
   (reader (ByteArrayInputStream. (.getBytes string charset)))))
