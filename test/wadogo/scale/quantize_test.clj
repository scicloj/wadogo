(ns wadogo.scale.quantize-test
  (:require [wadogo.scale :as s]
            [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]))

(deftest default-scale
  (let [q (s/scale :quantize)]
    (testing "everything to 0"
      (is (= (q -1) 0))
      (is (= (q 0) 0))
      (is (= (q 1) 0))
      (is (= (q 2) 0)))
    (testing "inverting to default interval"
      (is (= (s/inverse q 0) {:dstart 0.0, :dend 1.0, :id 0, :value 0})))))

;; https://github.com/d3/d3-scale/blob/master/test/quantize-test.js

(deftest split-into-two
  (let [q (s/scale :quantize {:domain [0.0 1.0]
                              :range [0 1]})]
    (testing "expected values"
      (is (= (s/domain q) [0.0 1.0]))
      (is (= (s/range q) [0 1]))
      (is (= (s/data q :thresholds) '(0.5)))
      (is (= (q 0.25) 0))
      (is (= (q 0.75) 1)))))

(deftest splits-evenly
  (let [q (s/scale :quantize {:range (range 3)})]
    (testing "expected values"
      (is (= (s/data q :thresholds) [(/ 3.0) (/ 2.0 3.0)]))
      (are [a v] (= a v)
        (q 0.0) 0
        (q 0.2) 0
        (q 0.4) 1
        (q 0.6) 1
        (q 0.8) 2
        (q 1.0) 2))
    (testing "clamping to the domain"
      (is (= (q -1) 0))
      (is (= (q 2) 2)))))

(deftest custom-domain
  (let [q (s/scale :quantize {:domain [-1.2 2.4]
                              :range [0 1]})]
    (testing "has expected split"
      (is (= (q -1.2) 0))
      (is (= (q 0) 0))
      (is (= (q 0.5999) 0))
      (is (= (q 0.6) 1))
      (is (= (q 2) 1)))))

(deftest multiple-valued-domain
  (let [q (s/scale :quantize {:domain [1.0 -1.2 2.4]
                              :range [0 1]})]
    (testing "min and max values as an interval"
      (is (= (s/domain q) [-1.2 2.4]))
      (is (= (q -1.2) 0))
      (is (= (q 0) 0))
      (is (= (q 0.5999) 0))
      (is (= (q 0.6) 1))
      (is (= (q 2) 1)))))

(deftest different-ranges
  (let [q (s/scale :quantize)]
    (testing "cardinality"
      (is (m/approx-eq ((s/with-range q (range 0 1.001 0.001)) 1/3) 0.333))
      (is (m/approx-eq ((s/with-range q (range 0 1.01 0.01)) 1/3) 0.33))
      (is (m/approx-eq ((s/with-range q (range 0 1.1 0.1)) 1/3) 0.4)) ;; rounding error
      (is (m/approx-eq ((s/with-range q (range 0 1.2 0.2)) 1/3) 0.4))
      (is (m/approx-eq ((s/with-range q (range 0 1.25 0.25)) 1/3) 0.25))
      (is (m/approx-eq ((s/with-range q (range 0 1.5 0.5)) 1/3) 0.5)))))

(deftest inverting
  (let [q (s/scale :quantize {:range [:a :b :c :d]})]
    (testing "returns a map"
      (is (= (s/inverse q :a) {:dstart 0.0, :dend 0.25, :id 0, :value :a}))
      (is (= (s/inverse q :b) {:dstart 0.25, :dend 0.5, :id 1, :value :b}))
      (is (= (s/inverse q :c) {:dstart 0.5, :dend 0.75, :id 2, :value :c}))
      (is (= (s/inverse q :d) {:dstart 0.75, :dend 1.0, :id 3, :value :d})))))
