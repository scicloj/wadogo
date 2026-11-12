(ns wadogo.scale.attributes-test
  "Shared attributes `:clamp`, `:unknown` and `:round`, degenerate domains (d3 cases are in `d3-test`)."
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.core :as m]
            [wadogo.scale :as s])
  (:import [java.time LocalDateTime LocalDate]))

(def continuous-kinds
  "Continuous kinds with a domain spanning [1, 4] and range [0, 10] (identity: domain [1, 4])."
  {:linear {:domain [1 4] :range [0 10]}
   :identity {:domain [1 4]}
   :pow {:domain [1 4] :range [0 10] :exponent 2}
   :log {:domain [1 4] :range [0 10]}
   :symlog {:domain [1 4] :range [0 10]}
   :radial {:domain [1 4] :range [0 10]}
   :interpolated {:domain [1 2 4] :range [0 5 10]}
   :sequential {:domain [1 4]}
   :diverging {:domain [1 2 4]}})

(deftest clamp-test
  (doseq [[k attrs] continuous-kinds
          :let [sc (s/scale k (assoc attrs :clamp true))
                free (s/scale k attrs)]]
    (testing (str k)
      (is (== (free 1) (sc 0.5)) "below")
      (is (== (free 4) (sc 10)) "above")
      (is (== (free 2.5) (sc 2.5)) "inside")
      (is (== 1.0 (double (s/inverse sc (- (double (free 1)) 100.0)))) "inverse below")
      (is (== 4.0 (double (s/inverse sc (+ (double (free 4)) 100.0)))) "inverse above"))))

(deftest clamp-datetime-test
  (let [dt1 (LocalDateTime/of 2012 5 5 10 10 10)
        dt2 (LocalDateTime/of 2012 12 5 10 10 10)
        sc (s/scale :datetime {:domain [dt1 dt2] :clamp true})]
    (is (== 1.0 (sc (LocalDateTime/of 2020 1 1 0 0))))
    (is (== 0.0 (sc (LocalDate/of 2000 1 1))))
    (is (= dt2 (s/inverse sc 2)))
    (is (= dt1 (s/inverse sc -2)))))

(deftest unknown-test
  (testing "continuous kinds: nil and NaN"
    (doseq [[k attrs] continuous-kinds
            :let [sc (s/scale k attrs)
                  u (s/scale k (assoc attrs :unknown :u))]]
      (is (nil? (sc nil)) (str k))
      (is (nil? (sc ##NaN)) (str k))
      (is (= :u (u nil) (u ##NaN)) (str k))))
  (testing "datetime"
    (is (= :u ((s/scale :datetime {:unknown :u}) nil))))
  (testing "continuous to discrete: nil and NaN"
    (doseq [[k attrs] {:quantize {} :threshold {:domain [0]}
                       :quantile {:domain [1 2 3]} :histogram {:domain [1 2 3]}}
            :let [sc (s/scale k attrs)
                  u (s/scale k (assoc attrs :unknown :u))]]
      (is (nil? (sc nil)) (str k))
      (is (nil? (sc ##NaN true)) (str k))
      (is (= :u (u nil) (u ##NaN) (u nil true)) (str k))))
  (testing "discrete domains: values outside the domain"
    (doseq [k [:bands :point :ordinal]
            :let [attrs {:domain [:a :b] :range [0 1]}
                  sc (s/scale k attrs)
                  u (s/scale k (assoc attrs :unknown :u))]]
      (is (nil? (sc :z)) (str k))
      (is (= :u (u :z) (u :z true) (u nil)) (str k)))))

(deftest round-test
  (doseq [[k attrs] (dissoc continuous-kinds :sequential :diverging :identity)
          :let [sc (s/scale k (assoc attrs :round true))
                free (s/scale k attrs)]]
    (doseq [x [1 1.3 2.5 3.7 4]]
      (is (== (m/floor (+ 0.5 (double (free x)))) (sc x)) (str k " " x))))
  (is (== 2.0 ((s/scale :identity {:domain [0 4] :round true}) 1.5)))
  (is (== 3.0 ((s/scale :sequential {:domain [0 1] :interpolator #(* 10.0 (double %)) :round true}) 0.26))))

(deftest degenerate-domain-test
  (testing "values below map to the range start, above to the range end, equal to the middle (d3: middle for all)"
    (doseq [k [:linear :log :pow :symlog]]
      (is (= [0.0 5.0 10.0] (mapv (s/scale k {:domain [5 5] :range [0 10]}) [4 5 6])) (str k)))
    (is (= [0.0 (m/sqrt 50.0) 10.0] (mapv (s/scale :radial {:domain [5 5] :range [0 10]}) [4 5 6])) "radial: middle of the squared range")
    (is (= [0.0 0.5 1.0] (mapv (s/scale :sequential {:domain [5 5]}) [4 5 6]))))
  (testing "degenerate range in the inverse"
    (is (= [0.0 0.5 1.0] (mapv #(s/inverse (s/scale :linear {:range [3 3]}) %) [2 3 4])))))

(deftest linear-domain-extent-test
  (testing "longer domains are reduced to minimum and maximum"
    (is (= [1.0 3.0] (s/domain (s/scale :linear {:domain [3 1 2]}))))
    (is (= [1.0 3.0] (s/domain (s/scale :pow {:domain [3 1 2]}))))
    (is (= [0.0 10.0] (s/range (s/scale :linear {:range [10 0 5]})))))
  (testing "two values keep their order"
    (is (= [3 1] (s/domain (s/scale :linear {:domain [3 1]}))))))
