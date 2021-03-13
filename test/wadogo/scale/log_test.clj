(ns wadogo.scale.log-test
  (:require [wadogo.scale :as s]
            [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]))

(deftest default-log-scale
  (let [l (s/scale :log)]

    (testing "default fields"
      (is (= (s/domain l) [1.0 10.0]))
      (is (= (s/range l) [0.0 1.0]))
      (is (= (s/data l :base) 10.0)))

    (testing "scaling"
      (is (m/approx-eq (l 5) 0.69897 5))
      (is (m/approx-eq (s/inverse l 0.69897) 5))
      (is (m/approx-eq (l 3.162278) 0.5))
      (is (m/approx-eq (s/inverse l 0.5) 3.162278 6)))

    (testing "outside the domain"
      (is (m/approx-eq (l 0.5) -0.3010299 6))
      (is (m/approx-eq (l 15) 1.1760913 7)))))

(deftest custom-log-scale
  (let [l (s/scale :log {:domain [1.0 2.0]})]

    (testing "forwarding"
      (is (m/approx-eq (l 0.5) -1.0))
      (is (m/approx-eq (l 1.0) 0.0))
      (is (m/approx-eq (l 1.5) 0.5849625 7))
      (is (m/approx-eq (l 2.0) 1.0))
      (is (m/approx-eq (l 2.5) 1.3219281 7)))

    (testing "inversing"
      (is (m/approx-eq (s/inverse l -1.0) 0.5))
      (is (m/approx-eq (s/inverse l 0.0) 1.0))
      (is (m/approx-eq (s/inverse l 0.5849625) 1.5))
      (is (m/approx-eq (s/inverse l 1.0) 2.0))
      (is (m/approx-eq (s/inverse l 1.3219281) 2.5)))))
