(ns wadogo.scale.threshold-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact =>] :as midje]))

(facts "default threshold"
  (let [t (s/scale :threshold)]
    (fact "forward maps everything to 0"
      (t -1) => 0
      (t 0) => 0
      (t 1) => 0
      (t 2) => 0)
    (fact "inverse maps 0 to a map"
      (s/inverse t 0) => {:dstart ##-Inf :dend ##Inf :id 0 :value 0})))

;; https://github.com/d3/d3-scale/blob/master/test/threshold-test.js

(facts "mid split"
  (let [t (s/scale :threshold {:domain [0.5]
                               :range [0 1]})]
    (fact "forward maps everything to 0"
      (t 0) => 0
      (t 0.49999) => 0
      (t 0.5) => 1
      (t 1) => 1)))


(facts "maps number to a discrete value"
  (let [t (s/scale :threshold {:domain [1/3 2/3]
                               :range [:a :b :c]})]
    (fact "splits properly"
      (t 0) => :a
      (t 0.2) => :a
      (t 0.4) => :b
      (t 0.6) => :b
      (t 0.8) => :c
      (t 1) => :c)
    (let [interval (juxt :dstart :dend)]
      (fact "inverts into range"
        (interval (s/inverse t :a)) => [##-Inf 1/3]
        (interval (s/inverse t :b)) => [1/3 2/3]
        (interval (s/inverse t :c)) => [2/3 ##Inf]))))
