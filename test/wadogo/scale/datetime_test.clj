(ns wadogo.scale.datetime-test
  (:require [clojure.test :refer [deftest is]]
            [wadogo.scale :as s])
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
