(ns wadogo.ticks.log-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.ticks.log :as sut]
            [wadogo.ticks.pretty :as pretty]
            [wadogo.oracle :refer [load-oracle close-seqs?]]))

;; oracle: d3-scale log ticks (utils/d3/d3_oracle.mjs), `:branch` is the grid d3 used:
;; all subdivisions of powers or powers of the base; both are candidate grids of `:pretty` log ticks
(def log-cases (load-oracle "d3_log_ticks.edn"))

(defn- on-abs-domain
  "Apply `f` to the positive sorted domain, map ticks back to the sign and order of `[a b]`."
  [f a b]
  (let [lo (min (abs a) (abs b))
        hi (max (abs a) (abs b))
        t (sort (if (neg? a) (map - (f lo hi)) (f lo hi)))]
    (if (> a b) (reverse t) t)))

(defn- all-subdivisions [lo hi base] (#'sut/subdivision-ticks lo hi (range 1 (long base)) base))
(defn- powers [lo hi n base] (#'sut/power-ticks lo hi n base))

(deftest d3-grids-oracle-test
  (is (>= (count log-cases) 100))
  (is (= #{:subdivisions :powers :fallback} (set (map :branch log-cases))))
  (doseq [{[a b] :domain :keys [n base branch ticks]} log-cases
          :when (not= branch :fallback)]
    (is (close-seqs? ticks (on-abs-domain (fn [lo hi] (if (= branch :subdivisions)
                                                        (all-subdivisions lo hi base)
                                                        (powers lo hi n base))) a b))
        (str [a b n base branch]))))

(defn- candidates
  [lo hi n base]
  (cond-> [(powers lo hi n base)]
    (== base 10.0) (conj (#'sut/subdivision-ticks lo hi [1 2 5] base) (#'sut/subdivision-ticks lo hi [1 5] base))
    (and (> base 2.0) (== base (Math/floor base))) (conj (all-subdivisions lo hi base))))

(deftest pretty-log-ticks-closest-count-test
  (doseq [{[a b] :domain :keys [n base]} log-cases
          :let [lo (min (abs a) (abs b))
                hi (max (abs a) (abs b))
                cs (candidates lo hi n base)
                got (sut/pretty-log-ticks a b n base)]]
    (testing [a b n base]
      (if (< (* 2 (apply max (map count cs))) n)
        (is (close-seqs? (on-abs-domain (fn [lo hi] (pretty/pretty-ticks lo hi n)) a b) got) "linear fallback")
        (is (every? #(<= (abs (- (count got) n)) (abs (- (count %) n))) cs) "closest number of ticks")))))

(deftest pretty-log-ticks-examples-test
  (testing "grid with number of ticks closest to n"
    (is (= [1.0 10.0 100.0 1000.0] (sut/pretty-log-ticks 1 1000 5 10.0)))
    (is (= [1.0 2.0 5.0 10.0 20.0 50.0 100.0 200.0 500.0 1000.0] (sut/pretty-log-ticks 1 1000 10 10.0)))
    (is (= 28 (count (sut/pretty-log-ticks 1 1000 20 10.0))))
    (is (= [5.0 10.0 50.0 100.0 500.0] (sut/pretty-log-ticks 3 700 5 10.0)))
    (is (= [-500.0 -100.0 -50.0 -10.0 -5.0] (sut/pretty-log-ticks -700 -3 5 10.0)))
    (is (= [6.0 7.0 8.0 9.0] (sut/pretty-log-ticks 6 9 5 10.0)))
    (is (= [1.0 16.0 256.0 4096.0 65536.0] (sut/pretty-log-ticks 1 65536 20 16.0))))
  (testing "linear fallback (R: pretty(c(7, 8)))"
    (is (= [7.0 7.2 7.4 7.6 7.8 8.0] (sut/pretty-log-ticks 7 8 5 10.0)))))

(deftest default-log-ticks-test
  (testing "number of ticks is capped by number of powers (no fractional powers)"
    (is (= [1.0 10.0 100.0 1000.0] (sut/log-ticks 1 1000 5 10.0)))
    (is (= [1.0 10.0 100.0 1000.0] (sut/log-ticks 1 1000 nil 10.0)))
    (is (= [1.0 100.0 10000.0] (sut/log-ticks 1 10000 2 10.0))))
  (testing "negative and reversed domains"
    (is (= [-1000.0 -100.0 -10.0 -1.0] (sut/log-ticks -1000 -1 nil 10.0)))
    (is (= [-1.0 -10.0 -100.0 -1000.0] (sut/log-ticks -1 -1000 nil 10.0)))
    (is (= [1000.0 100.0 10.0 1.0] (sut/log-ticks 1000 1 nil 10.0))))
  (testing "unchanged for base 2"
    (is (= (map #(Math/pow 2.0 %) (range 11)) (sut/log-ticks 1 1024 nil 2.0))))
  (testing "fewer than two powers inside: linear ticks"
    (is (= [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0] (sut/log-ticks 1 5 nil 10.0)))
    (is (= [2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0 18.0 20.0 22.0 24.0 26.0 28.0 30.0] (sut/log-ticks 2 30 nil 10.0))))
  (testing "exact powers of the base are kept, powers outside the domain are not"
    (is (= [1.0E-5 1.0E-4 0.001] (sut/log-ticks 1e-5 1e-3 nil 10.0)))
    (is (= [10000.0 100000.0] (sut/log-ticks 1000.0000001 100000 nil 10.0)))
    (is (= [1000.0 10000.0 100000.0] (sut/log-ticks 999.9999999 100000 nil 10.0)))
    (is (= [100.0 100000.0] (take 2 (sut/log-nice-bounds 999.9999999 100000 nil 10.0)))))
  (testing "no ticks"
    (is (= [] (sut/log-ticks 1 1000 0 10.0)))))
