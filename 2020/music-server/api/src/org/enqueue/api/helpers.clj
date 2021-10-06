(ns org.enqueue.api.helpers
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.edn     :as edn]))


(defn read-edn-resource
  "Read an EDN file from JVM resources.  Accepts optional :eval? keyword
  parameter to toggle evaluation of the EDN (default: false)."
  [path & {:keys [eval?]}]
  (let [eval (if eval? eval identity)]
    (some-> path io/resource slurp edn/read-string eval)))


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


(defmacro while-nil
  "Execute 'body' for each element 'i' in 'itt' until a non-nil value is
  returned then return it."
  [[i itt] & body]
  `(some (fn [~i] ~@body) ~itt))


(defmacro when-let*
  "Short circuiting when-let on multiple binding forms."
  [bindings & body]
  (let [form (first bindings)
        tst  (second bindings)
        rst  (subvec bindings 2)]
    (if (seq rst)
      `(when-let [~form ~tst]
         (when-let* [~@rst] ~@body))
      `(when-let [~form ~tst]
         ~@body))))
