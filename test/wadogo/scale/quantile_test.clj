(ns wadogo.scale.quantile-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact =>] :as midje]
            [fastmath.core :as m]))

;; https://github.comidje/d3/d3-scale/blob/master/test/quantile-test.js
(facts "4 quantiles `:r7`"
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [0 1 2 3]
                              :estimation-strategy :r7})
        mq (partial map q)]
    (fact "creates proper split"
      (mq [3, 6, 6.9, 7, 7.1]) => (midje/five-of 0)
      (mq [8 8.9]) => (midje/two-of 1)
      (mq [9, 9.1, 10, 13]) => (midje/four-of 2)
      (mq [14.9, 15, 15.1, 16, 20]) => (midje/five-of 3))

    (let [q2 (s/with-domain q [3, 6, 7, 8, 8, 9, 10, 13, 15, 16, 20])
          mq2 (partial map q2)]
      (fact "creates proper split (added 8)"
        (mq2 [3, 6, 6.9, 7, 7.1]) => (midje/five-of 0)
        (mq2 [8 8.9]) => (midje/two-of 1)
        (mq2 [9, 9.1, 10, 13]) => (midje/four-of 2)
        (mq2 [14.9, 15, 15.1, 16, 20]) => (midje/five-of 3)))

    (fact "works with NaNs"
      (q ##NaN) => nil)

    (fact "generates proper quantiles"
      (map (comp :quantile #(q % true)) [3 8 9 15]) => [0.25 0.5 0.75 1.0])))

(facts "quantile ranges"
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [0 1 2 3]
                              :estimation-strategy :r7})
        inversed (map (partial s/inverse q) (s/range q))]
    (fact "start and end"
      (map :dstart inversed) => [3.0 7.25 9.0 14.5]
      (map :dend inversed) => [7.25 9.0 14.5 20.0])
    (fact "quantile values"
      (s/data q :quantiles) => '([7.25 0.25] [9.0 0.5] [14.5 0.75]))

    (let [invert-with-range (fn [r]
                              (let [nq (s/with-range q r)]
                                (map (comp m/approx first) (s/data nq :quantiles))))]
      (fact "cardinality of range determines quantiles"
        (invert-with-range (range 4)) => [7.25 9.0 14.5]
        (invert-with-range (range 2)) => [9.0]
        (invert-with-range (range 5)) => [6.8, 8.0, 11.2, 15.2]
        (invert-with-range (range 6)) => [6.5, 8.0, 9.0, 13.0, 15.5]))))


(facts "extents"
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range (range 4)
                              :estimation-strategy :r7})
        extent (comp (juxt :dstart :dend) (partial s/inverse q))]
    (fact "invert returns info about the domain"
      (extent 0) => [3.0 7.25]
      (extent 1) => [7.25 9.0]
      (extent 2) => [9.0 14.5]
      (extent 3) => [14.5 20.0])

    (let [q2 (s/with-range q [:a :b])
          extent2 (comp (juxt :dstart :dend) (partial s/inverse q2))]
      (fact "invert with non-numerical range"
        (extent2 :a) => [3.0 9.0]
        (extent2 :b) => [9.0 20.0]))

    (fact "invert outside range returns nil"
      (s/inverse q nil) => nil
      (s/inverse q -1) => nil
      (s/inverse q ##NaN) => nil)))

(facts "repeated range"
  (let [q (s/scale :quantile {:domain [3, 6, 7, 8, 8, 10, 13, 15, 16, 20]
                              :range [:a :b :c :a]
                              :estimation-strategy :r7})
        mq (partial map q)
        extent (comp (juxt :dstart :dend) (partial s/inverse q))]
    (fact "aware of duplicated values in range"
      (mq [3, 6, 6.9, 7, 7.1]) => (midje/five-of :a)
      (mq [8 8.9]) => (midje/two-of :b)
      (mq [9, 9.1, 10, 13]) => (midje/four-of :c)
      (mq [14.9, 15, 15.1, 16, 20]) => (midje/five-of :a))
    (fact "inverse removes duplicats and returns first possible range"
      (extent :a) => [3.0 7.25]
      (extent :b) => [7.25 9.0]
      (extent :c) => [9.0 14.5])))
