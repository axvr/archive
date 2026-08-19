(ns org.enqueue.api.docs-test
  (:require [clojure.test :refer :all]
            [org.enqueue.api.docs :as docs]
            [clojure.java.io :as io]))

(def doc-files
  (->> "docs"
       io/resource
       io/file
       file-seq
       (filter #(. % isFile))))

;; TODO
(deftest ^:integration generate-doc
  (doseq [f doc-files]
    (let [file (.getName f)]
      (testing (str "Generates doc page from " file)
        (let [doc (docs/get-docs-response file nil)]
          (is doc)
          (is (= 200 (:status doc))))))))
