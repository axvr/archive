(ns snakes-and-snakes
  "Analysing how many turns it will take to win a game of \"Snakes & Snakes\".

  https://www.youtube.com/watch?v=k2ixp5VozIs"
  (:require [clojure.string :as str]
            [clojure.pprint :refer [print-table]]))

;; 2025-09-24
;; Public domain.  No rights reserved.

(defn new-pos [pos roll]
  (let [r+p (+ pos roll)
        pos (if (> r+p 100)
              (- 100 (mod r+p 100))
              r+p)]
    ({14  4, 17  7, 31  9, 38 20
      54 34, 59 40, 62 19, 64 60
      67 51, 81 63, 84 28, 87 24
      91 71, 93 73, 95 75, 99 78
      100 :win}
     pos
     pos)))

(defn roll []
  (rand-nth (range 1 7)))

(defn play
  ([] (play {:pos 1, :turns 0, :snakes 0}))
  ([{:keys [pos turns snakes]}]
   (let [p (new-pos pos (roll))
         t (inc turns)]
     (if (identical? :win p)
       {:turns t, :snakes snakes}
       (recur {:pos p, :turns t, :snakes (if (< p pos) (inc snakes) snakes)})))))

;; Turns:snakes ratio
(->> (repeatedly play)
     (take 100000)
     (sort-by :turns)
     #_(print-table)
     (transduce (comp
                 (map (juxt :turns :snakes))
                 (map #(str/join "," %)))
                (completing #(str/join "\n" [%1 %2]))
                "turns,snakes")
     (spit "stats.csv"))

;; Turn frequency
(->> (repeatedly play)
     (take 100000)
     (mapv :turns)
     (frequencies)
     (mapv (fn [[k v]] {:turns k, :freq v}))
     (sort-by :turns)
     (transduce (comp
                 (map (juxt :turns :freq))
                 (map #(str/join "," %)))
                (completing #(str/join "\n" [%1 %2]))
                "turns,freq")
     (spit "stats.csv"))

;; After simulating 100,000 games:
;;   Min: 20 turns
;;   Max: 7000+ turns
;;   50th percentile: <= 519 turns
;;
;; Result: 50% of games would finish in 520 turns or fewer.
