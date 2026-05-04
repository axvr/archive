(defonce ^:const VOID `void#)

(defmacro inspect [m]
  (into '(voidable) cat m))

(macroexpand '(inspect {:foo (+ 1 2)}))

;; deftype or defrecord?  Maybe defrecord
;; with overrides for collection methods.

(defprotocol VoidableCollection
  (make [vals]))

(extend-protocol VoidableCollection
  HashMap
  ())

(defmacro make-voidable [coll]
  (make))

(defrecord VoidableHashMap)

(defrecord VoidableArrayMap)

(defrecord VoidableVec)

(defrecord VoidableList []
  )

(deftype VoidableHashMap [] [])

;; maybe I need proxy of gen-class instead

;; PersistentVector
;;   - asTransient (ret custom tvec)
;;   -
;; TransientVector
;;   - conj (void check)
;;   - assocN (void check)

;; override asTransient
;; custom transient versions

(defn voidable-transient-vec [vec]
  (proxy [TransientVector] [vec]
    (conj [obj]
      (if (identical? VOID obj)
        this
        (proxy-super obj))
    (assocN [i val]
      (if (identical? VOID val)
        this
        (proxy-super i val))))))

;; TODO may need to override create and EMPTY?
(proxy [PersistentVector] []
  (asTransient []
    (voidable-transient-vec this))
  (assoc [i val]
    (if (identical? VOID val)
      this
      (proxy-super i val)))
  ()
  ...)

;; above is not possible due to Clojure's data structures being marked as
;; "final" which prevents subclassing them.
