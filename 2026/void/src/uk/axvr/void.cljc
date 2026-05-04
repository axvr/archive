(ns uk.axvr.void)

(defn- voidable' [coll]
  (let [void (keyword (str *ns*) (str `void#))]
    `(into ~(empty coll)
           ~(if (map? coll)
              `(remove (fn [[_k# v#]] (identical? ~void v#)))
              `(remove (fn [x#] (identical? ~void x#))))
           (let [~'void ~void]
             ~(if (seq? coll) `(reverse ~coll) coll)))))

(defmacro voidable
  "Wrap a data structure literal in this to make it voidable.  If the element
  or value of a key-value pair is the symbol `void`, the item will be
  \"voided\", i.e. removed, as if it were never written in the first place."
  [coll] (voidable' coll))

(defn- supervoidable' [coll]
  `(let [coll# (voidable ~coll)]
     (if (seq coll#) coll# ~'void)))

(defmacro supervoidable
  "Wrap a data structure literal in this to make it \"supervoidable\".  Acts as
  if `voidable` was used but if the resulting collection is empty, it is
  instead marked as void.

  This can only be used inside of a `voidable`."
  [coll] (supervoidable' coll))

(comment
  (voidable {:foo 1, :bar void})

  (voidable
   {:foo 1
    :bar (supervoidable {:biz void})})

  #voidable {:foo 1, :bar void}

  #voidable
  {:foo 1
   :bar #supervoidable {:biz void}}

  #voidable [1 void 2]

  #voidable `(foo bar ~void)
  )
