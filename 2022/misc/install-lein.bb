(ns install-lein
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import [java.io File FileOutputStream]))

;; Circa 2022-01-31

(def lein-version "2.9.8")

(defn download [url file]
  (with-open [remote (io/input-stream url)
              local  (FileOutputStream. file)]
    (io/copy remote local)))

(def lein (str (System/getenv "HOME") "/.local/bin/lein"))

(defn installed? [p]
  (let [file (File. p)]
    (.canExecute file)))

(defn current-lein-version []
  (when (installed? lein)
    (some->>
      (sh lein "version")
      :out
      (re-find #"^Leiningen ([\d\.]+) ")
      second)))

(defn lein-url [version]
  (str "https://raw.githubusercontent.com/technomancy/leiningen/" version "/bin/lein"))

(defn install-lein [version]
  (when (not= version (current-lein-version))
    (let [file (File. lein)
          url  (lein-url version)]
      (println "Installing Leiningen" version)
      (.mkdirs (.getParentFile file))
      (download url file)
      (.setExecutable file true)
      (println "Successfully installed Leiningen"))))

(install-lein lein-version)
