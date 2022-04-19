(ns org.enqueue.api.helpers
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.edn     :as edn])
  (:import [java.io PushbackReader]))


(defn read-edn-resource
  "Read an EDN file from JVM resources."
  [path]
  (when-let [res (some-> path io/resource)]
    (with-open [rdr (io/reader res)]
      (edn/read (PushbackReader. rdr)))))


(defn in?
  "Returns true if coll contains elm."
  [coll elm]
  (some #(= elm %) coll))


(defn date-compare
  "Compare date1 to date2 using op.  Example ops: < > <= >= ="
  [op date1 date2]
  (op (.compareTo date1 date2) 0))


(defn trim-end
  "Trims 'rem' from the end of 's'."
  ([s rem]
   (if (and (string? s)
            (string? rem)
            (str/ends-with? s rem))
     (subs s 0 (- (count s)
                  (count rem)))
     s)))


(defmacro when-let*
  "Short circuiting when-let on multiple binding forms."
  [bindings & body]
  (let [[form tst & rst] bindings]
    `(when-let [~form ~tst]
       ~(if (seq rst)
          `(when-let* ~rst ~@body)
          `(do ~@body)))))
