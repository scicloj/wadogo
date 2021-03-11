(ns wadogo.scale.linear-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact => roughly]]
            [midje.experimental :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [fastmath.core :as m]))

(facts "default linear scale"
  (let [l (s/scale :linear)]

    (fact "has defaults"
      (s/domain l) => [0.0 1.0]
      (s/range l) => [0.0 1.0]
      (s/kind l) => :linear)
    
    (fact "is identity"
      (for-all [n (gen/double* {:min 0.0 :max 1.0 :NaN? false})]
               {:num-tests 100}

               (l n) => n
               (s/forward l n) => n
               (s/inverse l n) => n))

    (fact "can be changed"
      (-> (s/with-domain l [1 2])
          (s/domain)) => [1 2]
      (-> (s/with-range l [-100 100])
          (s/range)) => [-100 100])))

(facts "custom linear scale"
  (let [l (s/scale :linear {:domain [0 100]
                            :range [0 200]})]

    (fact "has proper domain and range"
      (s/domain l) => [0 100]
      (s/range l) => [0 200])

    (fact "scales properly"
      (for-all [n (gen/double* {:min 0.0 :max 100.0 :infinite? false :NaN? false})]
               {:num-tests 100}

               (l n) => (roughly (* 2.0 n))
               (s/forward l n) => (roughly (* 2.0 n))
               (s/inverse l n) => (roughly (* 0.5 n))))))

(facts "zero length domain or range"
  (let [l (s/scale :linear {:domain [0 0]})]
    (fact "forwards to middle of the range"
      (l 0) => 0.5
      (l -1) => 0.0
      (l 1) => 1.0
      (s/inverse l 0) => 0.0
      (s/inverse l 0.5) => 0.0
      (s/inverse l 1.0) => 0.0))

  (let [l (s/scale :linear {:range [0 0]})]
    (fact "inverts to middle of the domain"
      (l 0) => 0.0
      (l 0.5) => 0.0
      (l 1) => 0.0
      (s/inverse l 0) => 0.5
      (s/inverse l -1) => 0.0
      (s/inverse l 1) => 1.0)))

(fact "accepts all double values"
  (let [l (s/scale :linear)]
    (l ##NaN) => m/nan?
    (l ##Inf) => ##Inf
    (l ##-Inf) => ##-Inf))
