(ns mermaid
  (:require [clojure.string :as str]))

(def direction
  {:left->right "LR"
   :right->left "RL"
   :top->bottom "TB"
   :bottom->top "BT"})

;; Graph

(defn build
  ([graph]
   (build graph {}))
  ([graph opts]
   (...)))

(comment
  (build graph {::direction (:left->right direction)
                :title      "My graph"}))

{:direction "LR"
 :title "Hello world"
 :graphdefs {:_ {:my-node {:shape "round-corners"
                           :label "My Node"}}
             :my-subgraph {:my-node}}
 :styles {}
 :graph  nil}
