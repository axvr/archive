(ns shouldwalk
  (:refer-clojure :exclude [extend])
  (:require [clojure.core.match :refer [match]]))

(defn ->m-body [body] `(fn [~'super ~'this] ~body))

;; [:error :does-not-understand]
;; [:error :not-implemented]

;; TODO: finish as basic Smalltalk object eDSL.
;; TODO: create a real pure implementation of ShouldWalk.

(defmacro ->obj [base & body]
  ;; {:pre [(assert (even? (count body)) "must provide an even number of forms")]}
  `(fn this# [& params#]
     ((match (vec params#) ~@(map #(%1 %2) (cycle [identity ->m-body]) body))
      ~base this#)))

(def =object
  (->obj nil
         [:extend method body] (->obj this method body params (apply super params))
         :else [:error :does-not-understand]))

(def =undefined
  (=object :extend '[:echo x] 'x))

(=undefined :echo "hello, world")

;; Keep super references around for reflection?

(def =base
  (obj [:echo x] x
       ;; [:methods ...]
       ;; [:chain [method & ms]] (reduce apply (apply this method) ms)
       [:extend [[method body] & defs]] (let [super this]
                                          (obj method body defs params (apply super params)))
       [:extend method body] (let [super this] (obj method body params (apply super params)))
       :else [:error :does-not-understand]))

(def =undefined
  #_(=base :extend [[:undefined?] true
                  [:false? false]
                  [:true? false]]))

(=undefined :undefined?)

(=undefined :extend [:undefined?] true)

(=base :echo "foo")
