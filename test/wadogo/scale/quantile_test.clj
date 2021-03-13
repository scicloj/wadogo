(ns wadogo.scale.quantile-test
  (:require [wadogo.scale :as s]
            [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]))

;; https://github.com/d3/d3-scale/blob/master/test/quantile-test.js

(deftest split-to-4-r7
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [0 1 2 3]
                              :estimation-strategy :r7})
        mq (partial map q)]
    (testing "split"
      (is (= (mq [3, 6, 6.9, 7, 7.1]) (repeat 5 0)))
      (is (= (mq [8 8.9]) (repeat 2 1)))
      (is (= (mq [9, 9.1, 10, 13]) (repeat 4 2)))
      (is (= (mq [14.9, 15, 15.1, 16, 20]) (repeat 5 3))))

    (let [q2 (s/with-domain q [3, 6, 7, 8, 8, 9, 10, 13, 15, 16, 20])
          mq2 (partial map q2)]
      (testing "split (added 8)"
        (is (= (mq2 [3, 6, 6.9, 7, 7.1]) (repeat 5 0)))
        (is (= (mq2 [8 8.9]) (repeat 2 1)))
        (is (= (mq2 [9, 9.1, 10, 13]) (repeat 4 2)))
        (is (= (mq2 [14.9, 15, 15.1, 16, 20]) (repeat 5 3)))))

    (testing "NaNs"
      (is (= (q ##NaN) nil)))

    (testing "quantile values"
      (is (= (map (comp :quantile #(q % true)) [3 8 9 15]) [0.25 0.5 0.75 1.0])))))

(deftest quantile-ranges
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [0 1 2 3]
                              :estimation-strategy :r7})
        inversed (map (partial s/inverse q) (s/range q))]
    (testing "start and end"
      (is (= (map :dstart inversed) [3.0 7.25 9.0 14.5]))
      (is (= (map :dend inversed) [7.25 9.0 14.5 20.0])))
    (testing "quantile values"
      (is (= (s/data q :quantiles) '([7.25 0.25] [9.0 0.5] [14.5 0.75]))))

    (let [invert-with-range (fn [r]
                              (let [nq (s/with-range q r)]
                                (map (comp m/approx first) (s/data nq :quantiles))))]
      (testing "cardinality of range determines quantiles"
        (is (= (invert-with-range (range 4)) [7.25 9.0 14.5]))
        (is (= (invert-with-range (range 2)) [9.0]))
        (is (= (invert-with-range (range 5)) [6.8, 8.0, 11.2, 15.2]))
        (is (= (invert-with-range (range 6)) [6.5, 8.0, 9.0, 13.0, 15.5]))))))


(deftest extents
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range (range 4)
                              :estimation-strategy :r7})
        extent (comp (juxt :dstart :dend) (partial s/inverse q))]
    (testing "invert returns info about the domain"
      (is (= (extent 0) [3.0 7.25]))
      (is (= (extent 1) [7.25 9.0]))
      (is (= (extent 2) [9.0 14.5]))
      (is (= (extent 3) [14.5 20.0])))

    (let [q2 (s/with-range q [:a :b])
          extent2 (comp (juxt :dstart :dend) (partial s/inverse q2))]
      (testing "invert with non-numerical range"
        (is (= (extent2 :a) [3.0 9.0]))
        (is (= (extent2 :b) [9.0 20.0]))))

    (testing "invert outside range returns nil"
      (is (= (s/inverse q nil) nil))
      (is (= (s/inverse q -1) nil))
      (is (= (s/inverse q ##NaN) nil)))))

(deftest repeated-range
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [:a :b :c :a]
                              :estimation-strategy :r7})
        mq (partial map q)
        extent (comp (juxt :dstart :dend) (partial s/inverse q))]
    (testing "aware of duplicated values in range"
      (is (= (mq [3, 6, 6.9, 7, 7.1]) (repeat 5 :a)))
      (is (= (mq [8 8.9]) (repeat 2 :b)))
      (is (= (mq [9, 9.1, 10, 13]) (repeat 4 :c)))
      (is (= (mq [14.9, 15, 15.1, 16, 20]) (repeat 5 :a))))
    (testing "inverse removes duplicats and returns first possible range"
      (is (= (extent :a) [3.0 7.25]))
      (is (= (extent :b) [7.25 9.0]))
      (is (= (extent :c) [9.0 14.5])))))
