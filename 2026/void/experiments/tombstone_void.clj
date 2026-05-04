(ns uk.axvr.void
  :import (uk.axvr.void.V))

;; TODO: documentation.

(defonce ^:const VOID `void#)

;; (defmacro voidable)

;; TODO: needs a lot of overrides!
;; (def voidable-vec
;;   (proxy [clojure.lang.PersistentVector] []
;;     (assocN [i v]
;;             (if (identical? VOID v)
;;               ()))))

(defmacro inspect [c]
  (type (empty c)))

(defmacro inspect2 [c]
  (type c))

(inspect {:foo 1})
(inspect2 {:foo 1})

(defmacro make-voidable [coll]
  `(into ~(empty c)
         ;; (reject ...)
         ~coll))

#voidable {:foo 1}
->
(into {} (reject void-kv) [[:foo 1]])

;; (VoidableHashMap. :foo 1)

(defrecord VoidableMap []
  clojure.lang.ITransientMap
  clojure.lang.ITransientAssociative2
  ())

;; TODO: need to write these in Java :(
;; Transient collections can be proxy but not worth it?
