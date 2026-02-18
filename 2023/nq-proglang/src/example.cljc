(ns example
  (:require [org.enqueue.core :refer :all]
            [criterium.core :as cc]))

(println ">>>" "1")
(cc/quick-bench
 (->> (range 1 100000)
      (org.enqueue.core/map inc)
      (org.enqueue.core/map #(* 521 %))
      (org.enqueue.core/map inc)
      (org.enqueue.core/map #(* 12 %))
      (org.enqueue.core/filter even?)
      (into [])))
(flush)

(println ">>>" "2")
(cc/quick-bench
 (->> (range 1 100000)
      (org.enqueue.core/map inc)
      (org.enqueue.core/map #(* 521 %))
      (org.enqueue.core/map inc)
      (org.enqueue.core/map #(* 12 %))
      (into [] (org.enqueue.core/filter even?))))
(flush)

(println ">>>" "3")
(cc/quick-bench
 (into []
       (comp
        (org.enqueue.core/map inc)
        (org.enqueue.core/map #(* 521 %))
        (org.enqueue.core/map inc)
        (org.enqueue.core/map #(* 12 %))
        (org.enqueue.core/filter even?))
       (range 1 100000)))
(flush)

(println ">>>" "4")
(cc/quick-bench
 (->> (range 1 100000)
      (clojure.core/map inc)
      (clojure.core/map #(* 521 %))
      (clojure.core/map inc)
      (clojure.core/map #(* 12 %))
      (clojure.core/filter even?)
      (into [])))
(flush)

(println ">>>" "5")
(cc/quick-bench
 (into []
       (comp
        (clojure.core/map inc)
        (clojure.core/map #(* 521 %))
        (clojure.core/map inc)
        (clojure.core/map #(* 12 %))
        (clojure.core/filter even?))
      (range 1 100000)))
(flush)


(let [x (amb 1 2 3)
      y (amb 4 6 8)]
  (infer! even? x)
  (infer! #(= 8 (* %1 %2)) x y)
  (conclude @x))

(defprotocol Inferable
  (infer [this premise subjects] "")
  (conclude [this] "")
  (explain [this] "")
  (possibilities [this] ""))

(defrecord Amb [options inferences]
  Inferable
  (infer [_this] ...)
  (conclude [_this] ...)
  (explain [_this] ...))

(defn amb? [amb]
  (extends? Inferable amb))

(defn amb [& options]
  (ref (->Amb options nil)
       :validator amb?
       :meta {::amb options}))

(defn infer! [premise & subjects]
  (dosync
    (doseq [subject subjects]
      (alter subject infer premise subjects))))

::fallacy

;; dq nq
;; nq command name is taken.

;; dq build
;; dq connect

;; true false nil

;; decimal, integer, ratio, keyword, symbol, string, character,
;; list, vector, map, set, regex, comment

;; (with-span! {}
;;   )

(comment

  (ns uk.axvr.example
    "Hello, world!"
    {nq org.enqueue.core
     #{deftest} org.enqueue.test})

  (ns uk.axvr.example
    "Hello, world!"
    {org.enqueue.core {:refer :all}
     org.enqueue.test {:as test, :refer #{deftest}}})

  (ns uk.axvr.example
    "Hello, world!"
    (:require [org.enqueue.core :as nq]
              [org.enqueue.test :refer [deftest]]))

  )
