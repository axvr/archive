(ns collatz
  (:require [clojure.string :as str]))

;; 2023-12-28: Distracted by the Collatz conjecture.  After a bit of time
;; looking at it, I'd consider it to be undecidable, i.e. a derivative of the
;; halting problem, thus unprovable.

(defn step [x]
  (cond
    (= 1 x)   nil
    (even? x) (bit-shift-right x 1)
    :else     (inc (* 3 x))))

(defn collatz [init]
  (eduction (take-while some?)
            (iterate step init)))

(defn dec->hex [x]
  (.toUpperCase (Long/toHexString x)))

(defn dec->bin [x]
  (Long/toBinaryString x))

(comment
  (into []
        (comp
         (partition-by even?))
        (collatz 1235))
  )

;; Is the problem preventing it being solved, the operations that are applied?

;; Never get 2 odd numbers in a row?
;; Get lots of groups of even numbers which results in /2 /2 /2 /2

;; Only divisions by 2 that are odd, result in an increase.

;; Frequencies of each operation?
(->> (rand-int Integer/MAX_VALUE)
     collatz
     (into [] (map #(zero? (mod % 2))))
     frequencies
     ((juxt #(get % true 0) #(get % false 0)))
     (apply /)
     double)

;; There is >5/3x more halving than 3x+1 operations.  I.e. trends downwards.

(defn freq [x]
  (->> x
       collatz
       (into [] (map #(zero? (mod % 2))))
       frequencies
       ((juxt #(get % true 0) #(get % false 0)))
       (apply /)
       #_double))

(comment
  (mapv freq (range 5 100))

  (frequencies (mapv freq (range 5 100)))

  (->> (range 5 100)
       (mapv freq)
       frequencies
       #_(into {}))

  (reduce min (mapv freq (range 5 10000000)))
  )

;; Always 5/3:1 odds of halve:3x+1

;; (5/3)(x/2) : (3x+1)
;; 1.6666666(x/2) : (3x+1)

;; (5/3)(x/2) : 3x+1

;; (5x)/6 : 3x + 1

;; 5/3+ : 1
;; 1.66666+ : 1
;; 5/3/2 : 3+1

(double (/ 5 3 2)) : 4

;; 5/3 probably contains the most numbers in the sequence.

;; 5/3
;; x/2 : 3x+1

;; A => x / 2
;; B => 3 x + 1

;; 5A : 3B  (worst case)

;; 5x/2 : 3(3x + 1)
;; 5x/2 : 9x + 3
;; 5x = 18x + 6
;; 0 = 18x + 6 - 5x
;; -6 = 18x - 5x
;; -6 = 13x
;; x = -6/13
;; (double -6/13)


(defn step2 [x]
  (cond
    (= 1 x)   nil
    (even? x) (bit-shift-right x 1)
    :else     (recur (inc (* 3 x)))))

(defn collatz2 [init]
  (eduction (take-while some?)
            (iterate step2 init)))

(defn freq2 [x]
  (->> x
       collatz2
       (into [] (map #(zero? (mod % 2))))
       frequencies
       ((juxt #(get % true 0) #(get % false 0)))
       (apply /)
       double))

(partition-by even?
              (collatz2 11))
;; (mapv freq2 (range 5 100))

(require '[clojure.core.reducers :as r])

(time
 (r/fold (transient [])
         doall
         (pmap collatz2 (range 4 200))))

(collatz2)
