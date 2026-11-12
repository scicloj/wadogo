(ns wadogo.scale.discrete-test
  "Kinds with a discrete domain or range: bands, point, ordinal, constant, threshold, quantile (d3 cases are in `d3-test`)."
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.scale :as s]))

(deftest bands-inverse-test
  (let [b (s/scale :bands {:domain [:a :b :c] :range [0 100]})]
    (is (= [:a :b :c] (map #(s/inverse b %) [10 50 90])))
    (is (= [nil nil nil] (map #(s/inverse b %) [-5 105 nil]))))
  (let [b (s/scale :bands {:domain [:a :b :c] :range [0 100] :padding-in 0.5 :padding-out 0.5 :unknown :u})]
    (is (= [:u :b :u :u] (map #(s/inverse b %) [10 50 90 35])) "padding and outside give :unknown"))
  (let [b (s/scale :bands {:domain [:a :b :c] :range [100 0]})]
    (is (= [:c :b :a] (map #(s/inverse b %) [10 50 90])) "reversed range")))

(deftest bands-reversed-test
  (let [b (s/scale :bands {:domain [:a :b :c] :range [100 0]})
        {:keys [rstart rend point]} (b :a true)]
    (is (= 100.0 rstart) ":rstart is the end nearer the range start")
    (is (< rend rstart))
    (is (= point (/ (+ rstart rend) 2.0)))
    (is (= [83 17] (map #(Math/round (double %)) [(b :a) (b :c)])))))

(deftest bands-align-test
  (testing ":align is the point position inside the band"
    (let [b (s/scale :bands {:domain [:a :b] :range [0 100] :align 0.0})]
      (is (= [0.0 50.0] (map b [:a :b])))))
  (testing ":align-outer distributes the outer padding"
    (let [b (s/scale :bands {:domain [:a :b] :range [0 100] :padding-out 1 :align-outer 0.0})]
      (is (= 0.0 (:rstart (b :a true))))
      (is (= 25.0 (s/data b :bandwidth))))))

(deftest quantize-domain-test
  (is (= {:domain [10 0]} (try (s/scale :quantize {:domain [10 0] :range [:a :b]})
                               (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (= [:a :b] (map (s/scale :quantize {:domain [10 0 5] :range [:a :b]}) [1 9])) "longer domains: minimum and maximum"))

(deftest repeated-domain-values-test
  (is (= [25.0 75.0] (map (s/scale :bands {:domain [:a :b :a] :range [0 100]}) [:a :b])))
  (is (= [:a :b] (s/domain (s/scale :point {:domain [:a :b :a] :range [0 100]})))))

(deftest point-test
  (let [p (s/scale :point {:domain [:a :b :c] :range [0 100] :padding 0.5})]
    (is (= :b (s/inverse p 50)))
    (is (= :a (s/inverse p 17)))
    (is (nil? (s/inverse p 101)))
    (is (identical? p (s/nice p)))
    (is (= [:a :b :c] (s/ticks p)))
    (is (= 0.0 (s/data p :bandwidth)))
    (is (= [0.0 (/ 100.0 3.0)] (map (s/scale :point {:domain [:a :b] :range [0 100] :padding 1 :align-outer 0}) [:a :b])) "outer space after the last point")))

(deftest ordinal-test
  (let [o (s/scale :ordinal {:domain [:a :b :c :d] :range [1 2]})]
    (testing "range is cycled"
      (is (= [1 2 1 2] (map o [:a :b :c :d]))))
    (testing "values outside the domain"
      (is (nil? (o :e)))
      (is (nil? (o :e 5)) "second argument is ignored")
      (is (= :u ((s/with-data o :unknown :u) :e 5))))
    (testing "inverse returns the first domain value"
      (is (= :a (s/inverse o 1)))
      (is (= :b (s/inverse o 2)))
      (is (nil? (s/inverse o 3)))))
  (testing "sorting"
    (is (= {:domain [1 :a]} (try (s/scale :ordinal {:domain [1 :a]})
                                 (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (is (= [1 :a "x"] (s/domain (s/scale :ordinal {:domain [1 :a "x"] :sort? false}))))
    (is (= [3 2 1] (s/domain (s/scale :ordinal {:domain [3 1 2] :sort? >}))))
    (is (= [1 2 3] (s/domain (s/scale :ordinal {:domain [3 1 2 1]})))))
  (testing "missing domain or range"
    (is (= [0 1 2] (s/domain (s/scale :ordinal {:range [:x :y :z]}))))
    (is (= [0 1] (s/range (s/scale :ordinal {:domain [:p :q]}))))))

(deftest constant-test
  (let [c (s/scale :constant {:domain [1 2] :range 5})]
    (is (= [5 5 5 5] (map c [1 :x nil ##NaN])))
    (is (= [1 2] (s/inverse c 7)))
    (is (= [1 2] (s/ticks c)))
    (is (identical? c (s/nice c))))
  (is (nil? ((s/scale :constant) 1))))

(deftest threshold-domain-test
  (is (= {:domain [1 0]} (try (s/scale :threshold {:domain [1 0]})
                              (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (let [t (s/scale :threshold {:domain [0 0 1] :range [:a :b :c :d]})]
    (is (= [:a :c :c :d] (map t [-1 0 0.5 1])) "repeated thresholds give an empty interval"))
  (is (nil? ((s/scale :threshold {:domain [0 1] :range [:a]}) 0.5)) "shorter range"))

(deftest quantile-out-of-range-test
  (let [q (s/scale :quantile {:domain [3 6 7 8 8 10 13 15 16 20] :range [0 1 2 3]})]
    (is (= [0 0 3 3] (map q [-100 2.9 20.1 1e9])) "clamped to the first and last interval")
    (is (= [7.25 9.0 14.5] (map first (s/data q :quantiles))) "default :r7")
    (is (= [6.75 9.0 15.25] (map first (s/data (s/with-data q :estimation-strategy :legacy) :quantiles)))))
  (testing "repeated quantiles"
    (let [q (s/scale :quantile {:domain [1 2] :range 7})]
      (is (= 6 (count (s/data q :quantiles))))
      (is (= [(/ 1.0 7.0) 1.0] (map q [1 2]))))))

(deftest histogram-out-of-range-test
  (let [h (s/scale :histogram {:domain [1 2 3 4 5 6 7 7 8 9] :range 3})]
    (is (= [0 0 2 2] (map h [-100 1 9 100])))))
