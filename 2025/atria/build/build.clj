(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(def lib 'org.enqueue/catalyst)
(def version "local")
(def basis (b/create-basis {:project "deps.edn"}))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_opts]
  (b/delete {:path "target"}))

(defn compile [_opts]
  (b/compile-clj {:basis basis
                  :class-dir class-dir
                  :src-dirs ["src"]
                  :compile-opts {:direct-linking true}
                  :bindings {#'clojure.core/*warn-on-reflection* true}}))

(defn pom [_opts]
  (b/write-pom {:class-dir     class-dir
                :lib           lib
                :version       version
                :basis         basis
                :resource-dirs ["resources"]
                :src-dirs      ["src"]
                :src-pom       "build/pom.xml"}))

(defn uberjar
  [_opts]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (pom nil)
  (compile nil)
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           ;; :main      '...
           :basis     basis}))

(defn install [_opts]
  (b/install
    {:basis      basis
     :lib        lib
     :version    version
     :jar-file   jar-file
     :class-dir  class-dir}))
