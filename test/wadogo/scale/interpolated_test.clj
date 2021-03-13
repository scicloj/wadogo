(ns wadogo.scale.interpolated-test
  (:require [wadogo.scale :as s]
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
