(ns wadogo.scale.interpolated-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact =>]]))

(facts "linear interpolation"
  (let [l (s/scale :interpolated {:domain [4 2 1]
                                  :range [1 2 4]})]
    (fact "can be applied on descending domain"
      (l 1.5) => 3.0
      (l 3) => 1.5
      (s/inverse l 1.5) => 3.0
      (s/inverse l 3) => 1.5))

  (let [l (s/scale :interpolated {:domain [1 2 4]
                                  :range [4 2 1]})]
    (fact "can be applied on ascending domain"
      (l 1.5) => 3.0
      (l 3) => 1.5
      (s/inverse l 1.5) => 3.0
      (s/inverse l 3) => 1.5)))
