(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(def lib 'uk.axvr/void)

(def basis (delay (b/create-basis {:project "deps.edn"})))
(def class-dir "target/classes")

(defn clean [_opts]
  (b/delete {:path "target"}))

(defn compile [_opts]
  (b/javac {:src-dirs   ["src"]
            :class-dir  class-dir
            :basis      @basis
            :javac-opts ["--release" "21"]}))
