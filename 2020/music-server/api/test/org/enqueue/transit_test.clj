(ns org.enqueue.transit-test
  (:require [clojure.test :refer :all]
            [org.enqueue.transit :as transit]))


(def date-pattern
  (.. (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:MM:ss'.'SSSZ")
      (withZone (java.time.ZoneId/of "Europe/London"))))


(defn format-date [date]
  (.format date-pattern date))


(deftest instant
  (testing "Can encode and decode java.time.Instant types"
    (let [inst (java.time.Instant/now)
          encd (transit/encode inst)
          decd (transit/decode encd)]
      (is (= (type decd) java.time.Instant))
      (is (= (format-date inst) (format-date decd))))))


(deftest date
  (testing "If java.util.Date is encoded/decoded as a java.time.Instant"
    (let [date (java.util.Date.)
          encd (transit/encode date)
          decd (transit/decode encd)]
      (is (= (type decd) java.time.Instant))
      (is (= (format-date (.toInstant date)) (format-date decd))))))
