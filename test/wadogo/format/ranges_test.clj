(ns wadogo.format.ranges-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.format.ranges :refer [range-formatter]]
            [wadogo.scale :as s]))

(deftest range-formatter-test
  (let [f (range-formatter)]
    (is (= ["< 3.0" "≥ 3.0" "[10.0, 30.0)"] (map f [[##-Inf 3] [3 ##Inf] [10 30]]))))
  (let [f (range-formatter {:endpoints nil :digits 2})]
    (is (= ["(-∞, 3.00)" "[3.00, ∞)" "[10.00, 30.00)"] (map f [[##-Inf 3] [3 ##Inf] [10 30]])))))

(deftest threshold-intervals-format-test
  (let [sc (s/scale :threshold {:domain [0 1] :range :intervals})]
    (is (= ["< 0.0" "[0.0, 1.0)" "≥ 1.0"] (s/format sc)))
    (is (= ["< 0.00" "[0.00, 1.00)" "≥ 1.00"] (s/format (s/with-data sc :formatter-params {:digits 2}))))
    (is (= ["(-∞, 0.0)" "[0.0, 1.0)" "[1.0, ∞)"] (s/format (s/with-data sc :formatter-params {:endpoints nil}))))))
