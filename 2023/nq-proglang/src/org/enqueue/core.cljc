(ns org.enqueue.core
  (:refer-clojure :exclude [map filter]))

(defn map
  ([f]
   (clojure.core/map f))
  ([f coll]
   (eduction (map f) coll)))

(defn filter
  ([f]
   (clojure.core/filter f))
  ([f coll]
   (eduction (filter f) coll)))
