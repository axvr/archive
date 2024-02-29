(ns uk.axvr.gait)

(def genkey
  "`clojure.core/gensym` but returns a keyword."
  (comp keyword gensym #(str % "__")))

(defmacro with-genkeys
  "Bulk generate \"genkey\" values and bind them to each `sym`."
  [syms & body]
  `(let ~(into []
               (mapcat (juxt identity (comp genkey name)))
               syms)
     ~@body))

(defrecord Module [in out mods cons])

(def network
  "Initial example network.  This is complicated, but not intended to be
  written by hand.  Everything uses IDs to prevent renames breaking things."
  (atom
   (with-genkeys [nand_a nand_b nand_q
                  not_a not_q
                  and_a and_b and_q
                  or_a or_b or_q]
     {:colls {:uk.axvr.gait {:mods {:nand (map->Module
                                           {:in  {nand_a "a", nand_b "b"}
                                            :out {nand_q "q"}})
                                    :not  (with-genkeys [nand_1]
                                            (map->Module
                                             {:in   {not_a "a"}
                                              :out  {not_q "q"}
                                              :mods {nand_1 [:uk.axvr.gait :nand]}
                                              :cons {not_q [[nand_1 nand_a]
                                                            [nand_1 nand_b]]
                                                     [nand_1 nand_q] [not_q]}}))
                                    :and  (with-genkeys [nand_1 not_1]
                                            (map->Module
                                             {:in   {and_a "a", and_b "b"}
                                              :out  {and_q "q"}
                                              :mods {nand_1 [:uk.axvr.gait :nand]
                                                     not_1  [:uk.axvr.gait :not]}
                                              :cons {and_a [[nand_1 nand_a]]
                                                     and_b [[nand_1 nand_b]]
                                                     [nand_1 nand_q] [[not_1 not_a]]
                                                     [not_1 not_q] [and_q]}}))
                                    :or   (with-genkeys [not_1 not_2 nand_1]
                                            (map->Module
                                             {:in   {or_a "a", or_b "b"}
                                              :out  {or_q "q"}
                                              :mods {not_1  [:uk.axvr.gait :not]
                                                     not_2  [:uk.axvr.gait :not]
                                                     nand_1 [:uk.axvr.gait :nand]}
                                              :cons {or_a [[not_1 not_a]]
                                                     or_b [[not_2 not_a]]
                                                     [not_1 not_q] [nand_1 nand_a]
                                                     [not_2 not_q] [nand_1 nand_b]
                                                     [nand_1 nand_q] or_q}}))}}}})))

;; TODO: should node IDs be composite?  collection + module + node or entirely unique.

(defn advance [network collection module ins]
  (get-in network [:colls collection :mods module]))

(comment
  (advance @network :uk.axvr.gait :not {"a" 1})
  )

(comment

  ;; TODO: inputs and outputs should be ordered.

  ;; (defn add-module
  ;;   "Add a new module (or updated version of a module) into the given network."
  ;;   [network collection module]
  ;;   (update-in network [:colls collection :mods (:name module)]
  ;;              ;; TODO: verify that module changes won't break the network.
  ;;              ;; TODO: verify that module uses other modules correctly.
  ;;              (fn [prev]
  ;;                (if prev
  ;;                  nil
  ;;                  nil))))

  ;; TODO: Can this be simplified?  Perhaps I should create helpers for building these.
  ;; TODO: reference existing IDs?
  ;; TODO: this will be invoked multiple times during the iterative creation process.

  (add-module
   network
   :uk.axvr.gait
   {:name :xor
    :in   {:a "a", :b "b"}
    :out  {:q "q"}
    :mods {:nand_1 [:uk.axvr.gait :nand]
           :nand_2 [:uk.axvr.gait :nand]
           :nand_3 [:uk.axvr.gait :nand]
           :nand_4 [:uk.axvr.gait :nand]}
    :cons {:a [[:nand_1 :a]
               [:nand_2 :a]]
           :b [[:nand_1 :b]
               [:nand_3 :a]]
           [:nand_1 :q] [[:nand_2 :b]
                         [:nand_3 :b]]
           [:nand_2 :q] [[:nand_4 :a]]
           [:nand_3 :q] [[:nand_4 :b]]
           [:nand_4 :q] [:q]}})

  )
