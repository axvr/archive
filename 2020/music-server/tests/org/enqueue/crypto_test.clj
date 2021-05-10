(ns org.enqueue.crypto-test
  (:require [clojure.test :refer :all]
            [org.enqueue.crypto :as crypto]
            [clojure.string :as string]))


(defn rand-unicode-string []
  (String.
    (.getBytes
      (apply str
             (map char (take (rand-int 300)
                             (repeatedly #(rand-int 65536))))))
    "UTF-8"))


(deftest base64-encode
  (testing "base64 encoding works"
    (is (empty? (crypto/base64-encode "")))
    (is (= "Rm9v" (crypto/base64-encode "Foo")))
    (let [rand-string (rand-unicode-string)]
      (when-not (empty? rand-string)
        (is (not (= rand-string (crypto/base64-encode rand-string))))))))


(deftest base64-decode
  (testing "base64 decoding works"
    (is (empty? (crypto/base64-decode "")))
    (is (= "Foo" (crypto/base64-decode "Rm9v")))
    (let [rand-string (rand-unicode-string)
          encoded-str (crypto/base64-encode rand-string "UTF-8")
          decoded-str (crypto/base64-decode encoded-str "UTF-8")]
      (when-not (empty? rand-string)
        (is (not (= rand-string encoded-str)))
        (is (not (= encoded-str decoded-str)))
        (is (= rand-string decoded-str))))))


(deftest hash-password--hashed+can-verify
  (testing "password hashing works and is verifiable"
    (dotimes [i 10]
      (let [password (rand-unicode-string)
            hashed-password (crypto/hash-password password)]
        (is (not (string/ends-with? hashed-password "\u0000")))
        (is (not (= password hashed-password)))
        (is (= 172 (count hashed-password)))
        (is (not (= (crypto/base64-encode password) hashed-password)))
        (is (crypto/verify-password hashed-password password))))))
