(ns wadogo.scale.threshold-test
  (:require [wadogo.scale :as s]
            [clojure.test :refer [deftest testing is]]))

(deftest default-threshold
  (let [t (s/scale :threshold)]
    (testing "forwards everything to 0"
      (is (= (t -1) 0))
      (is (= (t 0) 0))
      (is (= (t 1) 0))
      (is (= (t 2) 0)))
    (testing "inversing  0 to a map"
      (is (= (s/inverse t 0) {:dstart ##-Inf :dend ##Inf :id 0 :value 0})))))

;; https://github.com/d3/d3-scale/blob/master/test/threshold-test.js

(deftest mid-split
  (let [t (s/scale :threshold {:domain [0.5]
                               :range [0 1]})]
    (testing "forwards everything to 0"
      (is (= (t 0) 0))
      (is (= (t 0.49999) 0))
      (is (= (t 0.5) 1))
      (is (= (t 1) 1)))))

(deftest mapping-number-to-a-discrete-value
  (let [t (s/scale :threshold {:domain [1/3 2/3]
                               :range [:a :b :c]})]
    (testing "spliting properly"
      (is (= (t 0) :a))
      (is (= (t 0.2) :a))
      (is (= (t 0.4) :b))
      (is (= (t 0.6) :b))
      (is (= (t 0.8) :c))
      (is (= (t 1) :c)))
    (let [interval (juxt :dstart :dend)]
      (testing "inverting into range"
        (is (= (interval (s/inverse t :a)) [##-Inf 1/3]))
        (is (= (interval (s/inverse t :b)) [1/3 2/3]))
        (is (= (interval (s/inverse t :c)) [2/3 ##Inf]))))))
