(ns wadogo.scale.datetime-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.scale :as s]
            [wadogo.format.datetime])
  (:import [java.time LocalDate LocalDateTime]))

(deftest datetime-formatter-test
  (let [sc (s/scale :datetime {:domain [(LocalDate/of 2012 5 5) (LocalDate/of 2012 9 9)] :formatter (fn [_] "X")})]
    (is (every? #{"X"} (s/format sc)))
    (is (seq (s/format sc)))
    (is (every? #{"X"} (s/format (s/with-domain sc [(LocalDate/of 2012 1 1) (LocalDate/of 2013 1 1)]))))
    (is (every? #{"X"} (s/format (s/nice sc))))))

(deftest datetime-non-positive-count-test
  (let [sc (s/scale :datetime {:domain [(LocalDateTime/of 2012 5 5 10 10) (LocalDateTime/of 2012 12 5 10 10)] :ticks 0})]
    (is (= [] (s/ticks sc)))
    (is (identical? sc (s/nice sc)))))

(def dt1 (LocalDateTime/of 2012 5 5 10 10 10))
(def dt2 (LocalDateTime/of 2012 12 5 10 10 10))

(deftest datetime-types-test
  (let [zone (java.time.ZoneId/of "Europe/Warsaw")
        z1 (java.time.ZonedDateTime/of dt1 zone)
        z2 (java.time.ZonedDateTime/of dt2 zone)
        utc [(LocalDateTime/of 2012 5 5 8 10 10) (LocalDateTime/of 2012 12 5 9 10 10)]]
    (doseq [[a b] [[(.toInstant z1) (.toInstant z2)] [z1 z2] [(.toOffsetDateTime z1) (.toOffsetDateTime z2)]]
            :let [sc (s/scale :datetime {:domain [a b]})
                  reference (s/scale :datetime {:domain utc})]]
      (testing (str (class a))
        (is (= utc (s/domain sc)) "converted to UTC")
        (is (= 0.0 (sc a)))
        (is (= 1.0 (sc b)))
        (is (= (s/ticks reference) (s/ticks sc)))
        (is (= (s/ticks (s/with-data reference :ticks-method :pretty)) (s/ticks (s/with-data sc :ticks-method :pretty))))
        (is (= (s/domain (s/nice reference)) (s/domain (s/nice sc))))
        (is (= (s/format reference) (s/format sc)))))))

(deftest datetime-reversed-test
  (let [f (s/scale :datetime {:domain [dt1 dt2]})
        r (s/scale :datetime {:domain [dt2 dt1]})]
    (is (= [dt2 dt1] (s/domain r)) "order kept")
    (is (= 1.0 (r dt1)))
    (is (= dt2 (s/inverse r 0.0)))
    (is (seq (s/ticks r)))
    (is (= (reverse (s/ticks f)) (s/ticks r)))
    (is (= (reverse (s/ticks (s/with-data f :ticks-method :pretty))) (s/ticks (s/with-data r :ticks-method :pretty))))
    (is (= (reverse (s/domain (s/nice f))) (s/domain (s/nice r)))))
  (is (= [dt1 dt2] (s/domain (s/scale :datetime {:domain [dt2 (LocalDateTime/of 2012 7 1 0 0) dt1]}))) "longer domains: earliest and latest"))

(deftest datetime-format-few-ticks-test
  (let [sc (s/scale :datetime {:domain [dt1 dt2]})]
    (is (= [] (s/format (s/with-ticks sc 0))))
    (is (= ["May"] (s/format sc [dt1])) "pattern by the domain span")
    (is (= ["May"] (s/format (s/with-ticks sc [dt1]))))
    (is (= ["2012-05-05 10:10:10"] (map (wadogo.format.datetime/time-format [dt1]) [dt1])))))

(deftest datetime-degenerate-test
  (let [d (LocalDateTime/of 2020 1 1 0 0)
        sc (s/scale :datetime {:domain [d d]})]
    (is (= [0.0 0.5 1.0] (map sc [(.minusDays d 1) d (.plusDays d 1)])))
    (is (= d (s/inverse sc 0.3)))
    (is (= [] (s/ticks sc)))
    (is (= [d d] (s/domain (s/nice sc)))))
  (let [d (LocalDateTime/of 2020 1 1 0 0)
        sc (s/scale :datetime {:domain [d (.plusDays d 2)] :range [1 1]})]
    (is (= [d (.plusDays d 1) (.plusDays d 2)] (map #(s/inverse sc %) [0 1 2])) "degenerate range")))
