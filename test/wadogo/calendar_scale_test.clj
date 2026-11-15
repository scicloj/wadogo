(ns wadogo.calendar-scale-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.scale :as s])
  (:import [java.time LocalDate LocalDateTime]))

(deftest calendar-scale-test
  (let [domain [(LocalDate/of 2016 1 10) (LocalDate/of 2016 12 20)]]
    (testing "datetime :pretty with :unit / :step"
      (is (= [(LocalDateTime/of 2016 4 1 0 0) (LocalDateTime/of 2016 7 1 0 0) (LocalDateTime/of 2016 10 1 0 0)]
             (vec (s/ticks (s/scale :datetime {:domain domain :ticks-method :pretty :ticks-params {:unit :months :step 3}})))))
      (is (= [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2017 1 1 0 0)]
             (vec (s/domain (s/nice (s/scale :datetime {:domain domain :ticks-method :pretty :ticks-params {:unit :months :step 3}})))))))
    (testing "calendar ticks with :default are refused"
      (is (thrown? clojure.lang.ExceptionInfo (s/ticks (s/scale :datetime {:domain domain :ticks-params {:unit :months}}))))
      (is (thrown? clojure.lang.ExceptionInfo (s/nice (s/scale :datetime {:domain domain :ticks-params {:unit :months}})))))))
