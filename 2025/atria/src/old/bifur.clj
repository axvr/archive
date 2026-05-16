(ns org.enqueue.experiments
  (:import (io.lacuna.bifurcan IMap Map)))

(defn *transient [^IMap m]
  (.linear m))

(defn *persistent! [^IMap m]
  (.forked m))

(defn *assoc [^IMap m k v]
  (.put m k v))

(-> Map/EMPTY
    *transient
    (*assoc :foo 1)
    (*assoc :bar {})
    (*assoc :biz '[a b c])
    *persistent!)

(defn transient? [^IMap m]
  (.isLinear m))

(def foo (*transient Map/EMPTY))

(*assoc foo :bsdfadsf 1)

(transient? foo)

(*persistent! foo)



