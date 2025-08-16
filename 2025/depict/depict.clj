(ns depict
  (:require [dictim.d2.compile :refer [d2]]))

(defn- parse-line [line]
  (into []
        (comp (partition-by #(= '| %))
              (remove #(= '(|) %)))
        line))

(defn ->dictim [depict]
  (into []
        (map parse-line)
        depict))

(defmacro depict [& lines]
  `'~(->dictim lines))

(comment

  (depict
   [person microwave food
    | open . start . stop / beep
    | heat]
   [person food | stir])

  ;; person microwave food: open, start, stop / beep : heat
  ;; person food: eat

  )
