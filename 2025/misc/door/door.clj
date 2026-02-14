(ns door
  (:require [babashka.http-client :as http]
            [clojure.math.combinatorics :as com]
            [clojure.data.json :as json]))

(def codes
  ["5PzX8hmlfl3KVGpvoa8vcP"
   "4ID2wy4cZyvuTTeiVlqPaF"
   "My4HPkMkjWoqxM1uc1fjA"
   "6Pn76dIDtq3qIfOOSDitDk"
   "4Jf4ilpgnI9eS8UuiBFPOB"
   "7MDJECqb4rN1FbkNj3QO4n"])

(doseq [perm (mapv vec (com/permuted-combinations codes 4))]
  (->  "https://.../api/puzzle/validate-solution"
       (http/post {:headers {"Content-Type" "application/json"}
                   :body (json/write-str {"roomId" "3tWseRx0jUlu4cP3VM3KOL"
                                          "answer" perm})})
       :body
       json/read-str
       (get-in ["data" "isCorrect"])
       (str ": " perm)
       println))

;; Solution:
["6Pn76dIDtq3qIfOOSDitDk"
 "4ID2wy4cZyvuTTeiVlqPaF"
 "4Jf4ilpgnI9eS8UuiBFPOB"
 "My4HPkMkjWoqxM1uc1fjA"]

;; let g:conjure#log#hud#ignore_low_priority = v:true
