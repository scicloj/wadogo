(ns wadogo.scale.linear-test
  (:require [wadogo.scale :as s]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [fastmath.core :as m]
            [clojure.test :refer [deftest testing is]]))

(deftest default-linear-scale
  (let [l (s/scale :linear)]

    (testing "field values"
      (is (= (s/domain l) [0.0 1.0]))
      (is (= (s/range l) [0.0 1.0]))
      (is (= (s/kind l) :linear)))
    
    (testing "identity"
      (is (:pass? (tc/quick-check
                   100
                   (prop/for-all* [(gen/double* {:min 0.0 :max 1.0 :NaN? false})]
                                  (fn [n] (and (= (l n) n)
                                              (= (s/forward l n) n)
                                              (= (s/inverse l n) n))))))))

    (testing "field modification"
      (is (= (-> (s/with-domain l [1 2])
                 (s/domain)) [1 2]))
      (is (= (-> (s/with-range l [-100 100])
                 (s/range)) [-100 100])))))

(deftest custom-linear-scale
  (let [l (s/scale :linear {:domain [0 100]
                            :range [0 200]})]

    (testing "proper domain and range"
      (is (= (s/domain l) [0 100]))
      (is (= (s/range l) [0 200])))

    (testing "scaling"
      (is (:pass? (tc/quick-check
                   100
                   (prop/for-all* [(gen/double* {:min 0.0 :max 100.0 :infinite? false :NaN? false})]
                                  (fn [n] (let [acc 6
                                               n (m/approx n acc)
                                               f1 (m/approx (l n) acc)
                                               f2 (m/approx (s/forward l n) acc)
                                               i (m/approx (s/inverse l n) acc)
                                               fres (m/approx (* 2.0 n) acc)
                                               ires (m/approx (* 0.5 n) acc)
                                               res (and (== f1 f2 fres)
                                                        (== i ires))]
                                           #_(when-not res (println [(s/forward l n)
                                                                     (s/inverse l n)
                                                                     (* 2.0 n) (* 0.5 n)]
                                                                    [f1 f2 i fres ires]))
                                           res)))))))))

(deftest zero-length-domain-or-range
  (let [l (s/scale :linear {:domain [0 0]})]
    (testing "forwarding to middle of the range"
      (is (= (l 0) 0.5))
      (is (= (l -1) 0.0))
      (is (= (l 1) 1.0))
      (is (= (s/inverse l 0) 0.0))
      (is (= (s/inverse l 0.5) 0.0))
      (is (= (s/inverse l 1.0) 0.0))))

  (let [l (s/scale :linear {:range [0 0]})]
    (testing "inverting to the middle of the domain"
      (is (= (l 0) 0.0))
      (is (= (l 0.5) 0.0))
      (is (= (l 1) 0.0))
      (is (= (s/inverse l 0) 0.5))
      (is (= (s/inverse l -1) 0.0))
      (is (= (s/inverse l 1) 1.0)))))

(deftest invalid-doubles
  (let [l (s/scale :linear)]
    (is (m/nan? (l ##NaN)))
    (is (= (l ##Inf) ##Inf))
    (is (= (l ##-Inf) ##-Inf))))

(deftest descending-range
  (let [l (s/scale :linear {:domain [0 1] :range [100 0]})]
    (is (= (l 1) 0.0))
    (is (= (l 0) 100.0))))
