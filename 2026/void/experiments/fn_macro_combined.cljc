;; Combined macro and function.

;; NOTE this technically works, but breaks upon reevaluation.
(defn ^:macro voidable
  ([_&form _&env & coll] (voidable (first coll)))
  ([coll]
   (let [void (keyword (str *ns*) (str `void#))]
     `(into ~(empty coll)
            ~(if (map? coll)
               `(remove (fn [[_k# v#]] (identical? ~void v#)))
               `(remove (fn [x#] (identical? ~void x#))))
            (let [~'void ~void] ~(vec coll))))))

(defn ^:macro supervoidable
  ([_&form _&env & coll] (supervoidable (first coll)))
  ([coll]
   `(let [coll# (voidable ~coll)]
      (if (seq coll#) coll# ~'void))))
