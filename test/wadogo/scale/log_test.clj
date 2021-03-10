(ns wadogo.scale.log-test
  (:require [wadogo.scale :as s]
            [midje.sweet :refer [facts fact => roughly]]))

(facts "default log scale"
  (let [l (s/scale :log)]

    (fact "has defaults"
      (s/domain l) => [1.0 10.0]
      (s/range l) => [0.0 1.0]
      (s/data l :base) => 10.0)

    (fact "scales properly"
      (l 5) => (roughly 0.69897 5)
      (s/inverse l 0.69897) => (roughly 5)
      (l 3.162278) => (roughly 0.5)
      (s/inverse l 0.5) => (roughly 3.162278 6))

    (fact "calculates from outside the domain"
      (l 0.5) => (roughly -0.3010299 7)
      (l 15) => (roughly 1.1760913 7))))

(facts "custom domain for log scale"
  (let [l (s/scale :log {:domain [1.0 2.0]})]

    (fact "forwards"
      (l 0.5) => (roughly -1.0)
      (l 1.0) => (roughly 0.0)
      (l 1.5) => (roughly 0.5849625 7)
      (l 2.0) => (roughly 1.0)
      (l 2.5) => (roughly 1.3219281 7))

    (fact "inverses"
      (s/inverse l -1.0) => (roughly 0.5)
      (s/inverse l 0.0) => (roughly 1.0)
      (s/inverse l 0.5849625) => (roughly 1.5)
      (s/inverse l 1.0) => (roughly 2.0)
      (s/inverse l 1.3219281) => (roughly 2.5))))

