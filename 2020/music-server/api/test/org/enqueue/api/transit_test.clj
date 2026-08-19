(ns org.enqueue.api.transit-test
  (:require [clojure.test :refer :all]
            [org.enqueue.api.transit :as transit]))

(def date-pattern
  (.. (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:MM:ss'.'SSSZ")
      (withZone (java.time.ZoneId/of "Europe/London"))))

(defn format-date [date]
  (.format date-pattern date))

(deftest instant-handlers
  (testing "Can encode a java.time.Instant type and decode back again."
    (let [inst (java.time.Instant/now)
          encd (transit/encode inst)
          decd (transit/decode encd)]
      (is (= (type decd) java.time.Instant))
      (is (= (format-date inst) (format-date decd)))))
  (testing "Can decode a previously encoded instant and encode back to it."
    (let [encoded "[\"~#'\",\"~t2021-08-11T15:26:09.014Z\"]"
          decoded (transit/decode encoded)]
      (is (= (class decoded) java.time.Instant))
      (is (= decoded (java.time.Instant/parse "2021-08-11T15:26:09.014Z")))
      (is (= encoded (transit/encode decoded))))))

(deftest date-handlers
  (testing "Can encode a java.util.Date type and decode it back to
           a java.time.Instant type."
    (let [date (java.util.Date.)
          encd (transit/encode date)
          decd (transit/decode encd)]
      (is (= (class decd) java.time.Instant))
      (is (= (format-date (.toInstant date)) (format-date decd)))))
  (testing "Can decode a previously encoded date."
    (let [encoded "[\"~#'\",\"~m1628695522236\"]"
          decoded (transit/decode encoded)]
      (is (= (class decoded) java.time.Instant))
      (is (= decoded (java.time.Instant/ofEpochMilli 1628695522236))))))

(deftest duration-handlers
  (testing "Can encode and decode java.time.Duration types into custom Transit
           extension type."
    (let [duration (java.time.Duration/ofMillis (rand-int 1000000000))
          encoded  (transit/encode duration)
          decoded  (transit/decode encoded)]
      (is (= (class decoded) java.time.Duration))
      (is (not= decoded encoded))
      (is (= decoded duration))))
  (testing "Using correct tag for Transit duration extension type."
    (let [decoded (transit/decode "[\"~#dur\",\"86400000000000\"]")]
      (is (= (class decoded) java.time.Duration))
      (is (= decoded (java.time.Duration/ofDays 1)))))
  (testing "Decodes zero duration."
    (let [decoded (transit/decode "[\"~#dur\",\"0\"]")]
      (is (= (class decoded) java.time.Duration))
      (is (.isZero decoded))))
  (testing "Decodes negative durations."
    (let [decoded (transit/decode "[\"~#dur\",\"-374000000000\"]")]
      (is (= (class decoded) java.time.Duration))
      (is (= decoded (java.time.Duration/ofSeconds -374)))
      (is (.isNegative decoded)))))
