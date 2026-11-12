(ns wadogo.scale.continuous-test
  "Continuous kinds: symlog constant, log domain, interpolated inverse, identity, radial, sequential, diverging (d3 cases are in `d3-test`)."
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.core :as m]
            [fastmath.interpolation :as i]
            [wadogo.scale :as s]))

(deftest symlog-base-ignored-test
  (is (= [5.0 5.0] (s/domain (s/scale :symlog {:domain [5] :base 2}))) ":base doesn't complete a one-value domain"))

(deftest symlog-constant-test
  (is (= 1.0 (s/data (s/scale :symlog) :C)))
  (let [sc (s/scale :symlog {:domain [-1000 1000]})]
    (is (= (sc 10) ((s/with-data sc :base 2) 10)) ":base doesn't change the mapping")
    (is (m/delta-eq (/ (+ 1.0 (/ (m/log1p 10.0) (m/log1p 1000.0))) 2.0) (sc 10))))
  (is (not= ((s/scale :symlog {:domain [-1000 1000]}) 10)
            ((s/scale :symlog {:domain [-1000 1000] :C 10}) 10))))

(deftest log-domain-test
  (doseq [d [[0 10] [-1 10] [10 0] [0 0] [5 -5]]]
    (is (= {:domain d} (try (s/scale :log {:domain d}) (catch clojure.lang.ExceptionInfo e (ex-data e)))) (str d)))
  (is (= {:domain [0 10]} (try (s/with-domain (s/scale :log) [0 10]) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (= {:domain [0 10]} (try (s/scale :sequential {:transform :log :domain [0 10]}) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (m/delta-eq 0.5 ((s/scale :log {:domain [-100 -1]}) -10))))

(deftest interpolated-inverse-test
  (let [f (fn [xs ys] (i/interpolation :linear xs ys))
        sc (s/scale :interpolated {:domain [1 2 3] :range [10 20 40] :interpolator f})]
    (is (= 15.0 (sc 1.5)))
    (is (= 1.5 (s/inverse sc 15)))
    (is (= 2.25 (s/inverse sc 25))))
  (testing "loess with an options map"
    (let [sc (s/scale :interpolated {:domain [0 1 2 3 4 5 6] :range [0 1 4 9 16 25 36]
                                     :interpolator :loess :interpolator-params [{:bandwidth 0.7}]})]
      (is (number? (sc 2.5))))))

(deftest identity-test
  (let [sc (s/scale :identity {:domain [0.3 9.7]})]
    (is (= 3.0 (sc 3) ((s/with-data sc :clamp true) 3)))
    (is (= 3.0 (s/inverse sc 3)))
    (is (= (s/domain sc) (s/range sc)))
    (is (= [0.0 10.0] (s/domain (s/nice sc)) (s/range (s/nice sc))))
    (is (= [2.0 4.0 6.0 8.0] (s/ticks (s/with-data sc :ticks-method :pretty))))
    (is (= [1 2] (s/domain (s/scale :identity {:range [1 2]}))) "range when the domain is missing")))

(deftest radial-test
  (let [sc (s/scale :radial {:range [0 10]})]
    (is (= 5.0 (sc 0.25)))
    (is (= 0.25 (s/inverse sc 5)))
    (is (= [0.0 0.5 1.0] (take-nth 5 (s/ticks sc))))))

(deftest sequential-size-test
  (is (nil? (s/size (s/scale :sequential {:interpolator (fn [t] [t t])}))))
  (is (= 1.0 (s/size (s/scale :sequential)))))

(deftest sequential-test
  (let [sc (s/scale :sequential {:domain [10 20]})]
    (is (= 0.5 (sc 15)))
    (is (= 1.5 (sc 25)) "extrapolated without clamp")
    (is (= 15.0 (s/inverse sc 0.5)))
    (is (= [0.0 1.0] (s/range sc))))
  (testing "interpolator"
    (let [sc (s/scale :sequential {:domain [0 10] :interpolator (fn [t] [t t])})]
      (is (= [0.5 0.5] (sc 5)))
      (is (= [[0.0 0.0] [1.0 1.0]] (s/range sc)))))
  (testing "transforms"
    (is (= [1.0 10.0] (s/domain (s/scale :sequential {:transform :log}))))
    (is (= [1.0 10.0 100.0 1000.0] (s/ticks (s/scale :sequential {:domain [1 1000] :transform :log}))))
    (is (= [1.0 100.0] (s/domain (s/nice (s/scale :sequential {:domain [3 97] :transform :log})))))
    (is (m/delta-eq 0.25 ((s/scale :sequential {:domain [0 2] :transform :pow :exponent 2}) 1))))
  (testing "validation"
    (is (= {:interpolator :viridis} (try (s/scale :sequential {:interpolator :viridis}) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (is (= {:transform :foo} (try (s/scale :sequential {:transform :foo}) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(deftest diverging-test
  (let [sc (s/scale :diverging {:domain [-3 0 9]})]
    (is (= [0.0 0.5 1.0 0.75] (map sc [-3 0 9 4.5])))
    (is (= [-3.0 -1.5 0.0 9.0] (map #(s/inverse sc %) [0 0.25 0.5 1])))
    (is (= [-4.0 0.2 10.0] (s/domain (s/nice (s/scale :diverging {:domain [-3.3 0.2 9.7]}))))))
  (is (= [0.1 1.0 10.0] (s/domain (s/scale :diverging {:transform :log}))))
  (is (= {:domain [1 2]} (try (s/scale :diverging {:domain [1 2]}) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (= {:interpolator 1} (try (s/scale :diverging {:interpolator 1}) (catch clojure.lang.ExceptionInfo e (ex-data e))))))
