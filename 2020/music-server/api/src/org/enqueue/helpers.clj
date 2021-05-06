(ns org.enqueue.helpers
  (:require [clojure.string :as string]))


(defn trim-end
  "Trims 'rem' from the end of 's'."
  ([s rem]
   (if (and (string? s)
            (string? rem)
            (string/ends-with? s rem))
     (subs s 0 (- (count s)
                  (count rem)))
     s)))


(defmacro while-nil
  "Execute 'body' for each element 'i' in 'itt' until a non-nil value is
  returned then return it."
  [[i itt] & body]
  `(loop [j# ~itt]
     (when (seq j#)
       (let [~i (first j#)
             res# (do ~@body)]
         (if (some? res#)
           res#
           (recur (rest j#)))))))
