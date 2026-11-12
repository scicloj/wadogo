(ns wadogo.scale.histogram-test
  (:require [clojure.test :refer [deftest is]]
            [wadogo.scale :as s]))

(def data [1 2 3 4 5 6 7 7 8 9])

(deftest histogram-default-bins-test
  (let [fd (s/data (s/scale :histogram {:domain data :range :freedman-diaconis}) :bins)]
    (is (= fd (s/data (s/scale :histogram {:domain data}) :bins)))
    (is (= fd (s/data (s/scale :histogram {:domain data :range :default}) :bins)))
    (is (= fd (s/data (s/scale :histogram {:domain data :range nil}) :bins)))))

(deftest histogram-other-ranges-test
  (is (= 3 (count (s/data (s/scale :histogram {:domain data :range 3}) :bins))))
  (is (= [:a :b] (s/range (s/scale :histogram {:domain data :range [:a :b]}))))
  (is (= 1 (count (s/data (s/scale :histogram {:domain [1 1 1 1]}) :bins)))))
