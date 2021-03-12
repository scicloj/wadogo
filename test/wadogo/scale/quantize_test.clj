(ns wadogo.scale.quantize-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact =>] :as midje]))

(facts "default scale"
  (let [q (s/scale :quantize)]
    (fact "maps everything to 0"
      (q -1) => 0
      (q 0) => 0
      (q 1) => 0
      (q 2) => 0)
    (fact "inverts to default interval"
      (s/inverse q 0) => {:dstart 0.0, :dend 1.0, :id 0, :value 0})))

;; https://github.com/d3/d3-scale/blob/master/test/quantize-test.js

(facts "split into two"
  (let [q (s/scale :quantize {:domain [0.0 1.0]
                              :range [0 1]})]
    (fact "has expected values"
      (s/domain q) => [0.0 1.0]
      (s/range q) => [0 1]
      (s/data q :thresholds) => '(0.5)
      (q 0.25) => 0
      (q 0.75) => 1)))

(facts "splits evenly"
  (let [q (s/scale :quantize {:range (range 3)})]
    (fact "has expected values"
      (s/data q :thresholds) => [(/ 3.0) (/ 2.0 3.0)]
      (q 0.0) => 0
      (q 0.2) => 0
      (q 0.4) => 1
      (q 0.6) => 1
      (q 0.8) => 2
      (q 1.0) => 2)
    (fact "clamps to the domain"
      (q -1) => 0
      (q 2) => 2)))

(facts "custom domain"
  (let [q (s/scale :quantize {:domain [-1.2 2.4]
                              :range [0 1]})]
    (fact "has expected split"
      (q -1.2) => 0
      (q 0) => 0
      (q 0.5999) => 0
      (q 0.6) => 1
      (q 2) => 1)))

(facts "custom domain with multiple values"
  (let [q (s/scale :quantize {:domain [1.0 -1.2 2.4]
                              :range [0 1]})]
    (fact "takes min and max values as an interval"
      (s/domain q) => [-1.2 2.4]
      (q -1.2) => 0
      (q 0) => 0
      (q 0.5999) => 0
      (q 0.6) => 1
      (q 2) => 1)))

(facts "different ranges"
  (let [q (s/scale :quantize)]
    (fact "cardinality is used"
      ((s/with-range q (range 0 1.001 0.001)) 1/3) => (midje/roughly 0.333)
      ((s/with-range q (range 0 1.01 0.01)) 1/3) => (midje/roughly 0.33)
      ((s/with-range q (range 0 1.1 0.1)) 1/3) => (midje/roughly 0.4) ;; rounding error
      ((s/with-range q (range 0 1.2 0.2)) 1/3) => (midje/roughly 0.4)
      ((s/with-range q (range 0 1.25 0.25)) 1/3) => (midje/roughly 0.25)
      ((s/with-range q (range 0 1.5 0.5)) 1/3) => (midje/roughly 0.5))))

(facts "inverting"
  (let [q (s/scale :quantize {:range [:a :b :c :d]})]
    (fact "returns a map"
      (s/inverse q :a) => {:dstart 0.0, :dend 0.25, :id 0, :value :a}
      (s/inverse q :b) => {:dstart 0.25, :dend 0.5, :id 1, :value :b}
      (s/inverse q :c) => {:dstart 0.5, :dend 0.75, :id 2, :value :c}
      (s/inverse q :d) => {:dstart 0.75, :dend 1.0, :id 3, :value :d})))
