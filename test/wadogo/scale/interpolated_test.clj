(ns wadogo.scale.interpolated-test
  (:require [wadogo.scale :as s]
            [fastmath.interpolation :as i]
            [clojure.test :refer [deftest testing is]]))

(deftest linear-interpolation
  (let [l (s/scale :interpolated {:domain [4 2 1]
                                  :range [1 2 4]})]
    (testing "descending domain"
      (is (= (l 1.5) 3.0))
      (is (= (l 3) 1.5))
      (is (= (s/inverse l 1.5) 3.0))
      (is (= (s/inverse l 3) 1.5))))

  (let [l (s/scale :interpolated {:domain [1 2 4]
                                  :range [4 2 1]})]
    (testing "ascending domain"
      (is (= (l 1.5) 3.0))
      (is (= (l 3) 1.5))
      (is (= (s/inverse l 1.5) 3.0))
      (is (= (s/inverse l 3) 1.5)))))

(deftest monotone-methods-test
  (doseq [k [:linear :pchip :monotone :step :step-before :step-after]
          :let [sc (s/scale :interpolated {:domain [1 2 4 6 10] :range [-7 0 2 4 20] :interpolator k})
                ys (map sc (range 1.0 10.0 0.05))]]
    (is (every? (fn [[a b]] (<= a b)) (partition 2 1 ys)) (str k " is monotone"))
    (is (= [-7.0 0.0 2.0 4.0 20.0] (map (comp double sc) [1 2 4 6 10])) (str k " passes through the points")))
  (testing "inverse of the forward curve (bisection)"
    (let [l (s/scale :interpolated {:domain [1 2 4 6 10] :range [-7 0 2 4 20]})]
      (doseq [k [:linear :pchip :monotone :akima]
              :let [sc (s/with-data l :interpolator k)]
              x (range 1.0 10.0 0.25)]
        (is (< (Math/abs (- x (double (s/inverse sc (sc x))))) 1.0e-12) (str k " " x)))
      (testing "step: plateau start, jump position, unreachable values"
        (let [sc (s/with-data l :interpolator :step-after)]
          (is (= [1.0 2.0 4.0 10.0] (map #(s/inverse sc %) [-7 0 2 20])))
          (is (== 2.0 (s/inverse sc -3)) "inside the jump at 2")
          (is (Double/isNaN (s/inverse sc 30)) "beyond the last plateau")))
      (testing "extrapolation and decreasing curves"
        (is (== 12.5 (s/inverse l 30)))
        (is (== 10.0 (s/inverse (s/with-data l :clamp true) 30)))
        (let [d (s/scale :interpolated {:domain [1 2 4] :range [30 20 0] :interpolator :pchip})]
          (is (every? #(< (Math/abs (- % (double (s/inverse d (d %))))) 1.0e-12) [1 1.5 3 4])))))))

(deftest inverse-outside-domain-test
  (let [p (s/scale :interpolated {:domain [1 2 4 6 10] :range [-7 0 2 4 20] :interpolator :pchip})]
    (testing "extrapolation reached before the curve turns back"
      (is (< (Math/abs (- 0.5 (double (s/inverse p (p 0.5))))) 1.0e-12))
      (is (< (Math/abs (- 10.5 (double (s/inverse p (p 10.5))))) 1.0e-12)))
    (testing "values beyond the turn"
      (is (Double/isNaN (s/inverse p -20)))
      (is (Double/isNaN (s/inverse p 1e9)))))
  (testing "forward giving NaN outside the domain"
    (let [nan-outside (fn [xs ys] (let [f (i/interpolation :linear xs ys)]
                                    (fn [x] (if (<= 1.0 x 4.0) (f x) ##NaN))))
          sc (s/scale :interpolated {:domain [1 2 4] :range [0 10 20] :interpolator nan-outside})]
      (is (Double/isNaN (s/inverse sc -5)))
      (is (Double/isNaN (s/inverse sc 30)))
      (is (== 1.5 (s/inverse sc 5)))))
  (testing "clamp gives the nearer domain end"
    (let [st (s/scale :interpolated {:domain [1 2 4 6 10] :range [-7 0 2 4 20] :interpolator :step-after :clamp true})]
      (is (= [1.0 10.0] (map #(s/inverse st %) [-30 30]))))
    (let [d (s/scale :interpolated {:domain [1 2 4] :range [30 20 0] :clamp true})]
      (is (= [1.0 4.0] (map #(s/inverse d %) [40 -10])) "decreasing")))
  (testing "extra domain values are dropped"
    (let [sc (s/scale :interpolated {:domain [1 2 100] :range [0 10] :clamp true})]
      (is (= [1 2] (s/domain sc)))
      (is (== 10.0 (sc 50))))))
