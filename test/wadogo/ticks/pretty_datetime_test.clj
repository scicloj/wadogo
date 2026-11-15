(ns wadogo.ticks.pretty-datetime-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.ticks.pretty-datetime :as sut]
            [wadogo.oracle :refer [load-oracle]])
  (:import [java.time LocalDateTime]))

;; oracle: R `pretty()` on UTC POSIXct (utils/pretty/pretty_oracle.R)
(def datetime-cases (load-oracle "pretty_datetime.edn"))

(defn- ldt [s] (LocalDateTime/parse s))

(deftest pretty-datetime-oracle-test
  (is (>= (count datetime-cases) 60))
  (doseq [{:keys [start end n inside pretty]} datetime-cases
          :let [start (ldt start) end (ldt end)]]
    (testing [(str start) (str end) n]
      (is (= (map ldt inside) (sut/pretty-datetime-ticks start end n)))
      (is (= (map ldt inside) (reverse (sut/pretty-datetime-ticks end start n))))
      (is (= [(ldt (first pretty)) (ldt (last pretty))]
             (take 2 (sut/pretty-datetime-nice-bounds start end n (sut/default-min-n n))))))))

(deftest pretty-datetime-edges-test
  (let [t (LocalDateTime/of 2012 5 5 10 0 0)]
    (testing "degenerate domain: the point itself if it's a tick"
      (is (= [t] (sut/pretty-datetime-ticks t t 5))))
    (testing "zero ticks requested"
      (is (vector? (sut/pretty-datetime-ticks t (.plusDays t 3) 0))))
    (testing "validation"
      (is (thrown? clojure.lang.ExceptionInfo (sut/pretty-datetime-ticks t (.plusDays t 3) -2)))
      (is (thrown? clojure.lang.ExceptionInfo (sut/pretty-datetime-ticks t (.plusDays t 3) 5 6))))))

(deftest calendar-ticks-test
  (let [lo (ldt "2016-01-10T00:00") hi (ldt "2016-12-20T00:00")]
    (testing "every 3 months, anchored at the year"
      (is (= (map ldt ["2016-04-01T00:00" "2016-07-01T00:00" "2016-10-01T00:00"]) (sut/calendar-ticks lo hi :months 3))))
    (testing "reversed domain: ticks follow the order"
      (is (= (map ldt ["2016-10-01T00:00" "2016-07-01T00:00" "2016-04-01T00:00"]) (sut/calendar-ticks hi lo :months 3))))
    (testing "nice bounds: the outer calendar ticks"
      (is (= [(ldt "2016-01-01T00:00") (ldt "2017-01-01T00:00")] (take 2 (sut/calendar-nice-bounds lo hi :months 3)))))
    (testing "step 1 month: every month start inside"
      (is (= 11 (count (sut/calendar-ticks lo hi :months 1)))))
    (testing "a tick exactly at a domain end is kept"
      (is (= (ldt "2016-01-01T00:00") (first (sut/calendar-ticks (ldt "2016-01-01T00:00") hi :months 1))))))
  (testing "anchors"
    (is (= (map ldt ["2016-01-11T00:00" "2016-01-18T00:00"]) (sut/calendar-ticks (ldt "2016-01-07T00:00") (ldt "2016-01-20T00:00") :weeks 1))
        "weeks on Monday")
    (is (= (map ldt ["2016-01-01T06:00" "2016-01-01T12:00" "2016-01-01T18:00"])
           (sut/calendar-ticks (ldt "2016-01-01T05:00") (ldt "2016-01-01T20:00") :hours 6))
        "hours from the day start")
    (is (= (map ldt ["2015-01-01T00:00" "2020-01-01T00:00"]) (sut/calendar-ticks (ldt "2012-03-01T00:00") (ldt "2023-01-01T00:00") :years 5))
        "years from the decade")
    (is (= (map ldt ["2000-01-01T00:00" "2025-01-01T00:00"]) (sut/calendar-ticks (ldt "1990-03-01T00:00") (ldt "2040-01-01T00:00") :years 25))
        "years over 10 from the century")
    (is (= (map ldt ["2016-01-01T00:00:15" "2016-01-01T00:00:30" "2016-01-01T00:00:45"])
           (sut/calendar-ticks (ldt "2016-01-01T00:00:10") (ldt "2016-01-01T00:00:50") :seconds 15))
        "seconds from the minute")
    (is (= (map ldt ["2016-01-01T10:20" "2016-01-01T10:40"]) (sut/calendar-ticks (ldt "2016-01-01T10:05") (ldt "2016-01-01T10:50") :minutes 20))
        "minutes from the hour")
    (is (= (map ldt ["2016-01-12T00:00" "2016-01-14T00:00"]) (sut/calendar-ticks (ldt "2016-01-10T12:00") (ldt "2016-01-14T00:00") :days 2))
        "days from the domain start's day"))
  (testing "degenerate domain on a tick"
    (let [t (ldt "2016-03-01T00:00")] (is (= [t] (sut/calendar-ticks t t :months 1)))))
  (testing "no tick inside"
    (is (= [] (sut/calendar-ticks (ldt "2016-03-02T00:00") (ldt "2016-03-20T00:00") :months 1))))
  (testing "validation"
    (is (thrown? clojure.lang.ExceptionInfo (sut/calendar-ticks (ldt "2016-01-01T00:00") (ldt "2017-01-01T00:00") :quarters 1)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/calendar-ticks (ldt "2016-01-01T00:00") (ldt "2017-01-01T00:00") :months 0)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/calendar-ticks (ldt "2016-01-01T00:00") (ldt "2017-01-01T00:00") :months 1.5)))))

(deftest calendar-cap-test
  (let [lo (ldt "2016-01-01T00:00") hi (ldt "2017-01-01T00:00")]
    (testing "the estimate bounds the count within 2"
      (is (<= 0 (- (sut/calendar-tick-estimate lo hi :days 1) (count (sut/calendar-ticks lo hi :days 1))) 2))
      (is (<= 0 (- (sut/calendar-tick-estimate hi lo :months 1) (count (sut/calendar-ticks hi lo :months 1))) 2)))
    (testing "more than max-calendar-ticks is refused"
      (is (= 10000 sut/max-calendar-ticks))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"more than 10000" (sut/calendar-ticks lo hi :minutes 1)))
      (is (thrown? clojure.lang.ExceptionInfo (sut/calendar-nice-bounds lo hi :seconds 1)))
      (is (= 8785 (count (sut/calendar-ticks lo hi :hours 1))) "under the cap: 366 days x 24 + 1"))
    (testing "a degenerate domain"
      (is (= 2 (sut/calendar-tick-estimate lo lo :seconds 1))))))
