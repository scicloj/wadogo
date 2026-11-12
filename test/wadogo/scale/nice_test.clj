(ns wadogo.scale.nice-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.scale :as s]
            [wadogo.oracle :refer [load-oracle]])
  (:import [java.time LocalDateTime]))

;; oracle: d3-scale `scaleLinear().nice(count)` (utils/d3/d3_oracle.mjs)
(def nice-cases (load-oracle "d3_linear_nice.edn"))

(defn- ends-are-ticks?
  [scale]
  (let [[a b] [(first (s/domain scale)) (last (s/domain scale))]
        t (set (s/ticks scale))]
    (boolean (and (t a) (t b)))))

(defn- unchanged? [scale niced] (= (s/->map scale) (s/->map niced)))

(deftest d3-linear-nice-oracle-test
  (is (>= (count nice-cases) 100))
  (doseq [{:keys [domain n nice]} nice-cases]
    (is (= (map double nice) (s/domain (s/nice (s/scale :linear {:domain domain :ticks n}))))
        (str [domain n]))))

(defn- random-domains
  [seed cnt]
  (let [r (java.util.Random. seed)]
    (repeatedly cnt #(let [a (* (- (.nextDouble r) 0.5) (Math/pow 10 (- (* 12 (.nextDouble r)) 4)))
                           b (+ a (* (.nextDouble r) (Math/pow 10 (- (* 12 (.nextDouble r)) 4))))
                           n (inc (.nextInt r 15))]
                       [[a b] n]))))

(deftest linear-ends-are-ticks-test
  (doseq [method [:default :pretty]]
    (testing method
      (let [cases (random-domains 42 10000)
            fails (for [[d n] cases
                        :let [sc (s/scale :linear {:domain d :ticks n :ticks-method method})
                              niced (s/nice sc)]
                        :when (not (ends-are-ticks? niced))]
                    [d n (unchanged? sc niced)])]
        ;; one tick: the step grows with every extension, d3 returns the scale unchanged
        (is (every? (fn [[_ n unchanged]] (and (== 1 n) unchanged)) fails) (pr-str (take 3 fails)))))))

(defn- random-log-domains
  [seed cnt]
  (let [r (java.util.Random. seed)]
    (repeatedly cnt #(let [lo (* (Math/pow 10 (- (* 10 (.nextDouble r)) 5)) (+ 0.5 (.nextDouble r)))
                           hi (* lo (Math/pow 10 (* 8 (.nextDouble r))))
                           n (when (.nextBoolean r) (inc (.nextInt r 12)))]
                       [[lo hi] n]))))

(defn- power-of? [^double base ^double x]
  (let [e (/ (Math/log x) (Math/log base))] (< (Math/abs (- e (Math/rint e))) 1.0e-9)))

(deftest log-ends-are-ticks-test
  (doseq [method [:default :pretty]
          base [10.0 2.0 Math/E 16.0]]
    (testing [method base]
      (let [fails (for [[d n] (random-log-domains 7 2000)
                        :let [sc (s/scale :log {:domain d :ticks n :ticks-method method :base base})
                              niced (s/nice sc)]
                        :when (not (ends-are-ticks? niced))]
                    {:n n :unchanged (unchanged? sc niced) :lower (first (s/domain niced))
                     :lower-tick? (contains? (set (s/ticks niced)) (first (s/domain niced)))
                     :upper-tick? (contains? (set (s/ticks niced)) (last (s/domain niced)))})]
        ;; allowed: no convergence (scale unchanged) or a linear grid reaching zero (lower end is a power of the base)
        (is (every? (fn [{:keys [unchanged lower lower-tick? upper-tick?]}]
                      (or unchanged (and upper-tick? (not lower-tick?) (power-of? base lower))))
                    fails)
            (pr-str (take 3 fails)))))))

(deftest nice-per-kind-test
  (testing "linear family"
    (is (= [0.1 1.0] (s/domain (s/nice (s/scale :linear {:domain [0.13 0.97]})))))
    (is (= [1.0 0.1] (s/domain (s/nice (s/scale :linear {:domain [0.97 0.13]})))))
    (is (= [0.0 100.0] (s/domain (s/nice (s/scale :pow {:domain [0.3 97]})))))
    (is (= [-100.0 1000.0] (s/domain (s/nice (s/scale :symlog {:domain [-13 970]})))))
    (is (= [0.1 0.5 1.0] (s/domain (s/nice (s/scale :interpolated {:domain [0.13 0.5 0.97] :range [0 0.2 1]})))))
    (is (= [1.0 0.5 0.1] (s/domain (s/nice (s/scale :interpolated {:domain [0.97 0.5 0.13] :range [0 0.2 1]}))))))
  (testing "pretty: repeated until stable (R's pretty() gives [-2 8] here, its ticks on [-2 8] are -5..10)"
    (let [niced (s/nice (s/scale :linear {:domain [-0.08029142 7.77878] :ticks 3 :ticks-method :pretty}))]
      (is (= [-5.0 10.0] (s/domain niced)))
      (is (= [-5.0 0.0 5.0 10.0] (s/ticks niced)))))
  (testing "log"
    (is (= [1.0 10000.0] (s/domain (s/nice (s/scale :log {:domain [3 7000]})))))
    ;; :pretty, 5 ticks: powers grid; 10 ticks: 1-2-5 grid, 3 and 7000 are not on it
    (is (= [1.0 10000.0] (s/domain (s/nice (s/scale :log {:domain [3 7000] :ticks-method :pretty})))))
    (is (= [3.0 7000.0] (s/domain (s/nice (s/scale :log {:domain [3 7000] :ticks-method :pretty :ticks 30})))))
    (is (= [1.0 5.0] (s/domain (s/nice (s/scale :log {:domain [1 5]})))))
    (is (= [-10000.0 -1.0] (s/domain (s/nice (s/scale :log {:domain [-7000 -3]})))))
    (is (= [10000.0 1.0] (s/domain (s/nice (s/scale :log {:domain [7000 3]}))))))
  (testing "datetime"
    (let [d [(LocalDateTime/of 2012 5 5 10 10) (LocalDateTime/of 2012 12 5 10 10)]]
      (is (= [(LocalDateTime/of 2012 5 1 0 0) (LocalDateTime/of 2013 1 1 0 0)]
             (s/domain (s/nice (s/scale :datetime {:domain d :ticks 5})))) "months")
      (is (= [(LocalDateTime/of 2012 5 5 0 0) (LocalDateTime/of 2012 12 6 0 0)]
             (s/domain (s/nice (s/scale :datetime {:domain d})))) "10 ticks: days")
      (let [niced (s/nice (s/scale :datetime {:domain d :ticks-method :pretty}))]
        (is (= [(LocalDateTime/of 2012 4 1 0 0) (LocalDateTime/of 2013 1 1 0 0)] (s/domain niced)))
        (is (ends-are-ticks? niced)))))
  (testing "custom scale"
    (s/defcustom :cube-test #(* % % %) #(Math/cbrt %))
    (is (= [0.1 1.0] (s/domain (s/nice (s/scale :cube-test {:domain [0.13 0.97]})))))))

(deftest nice-unchanged-test
  (testing "discrete domains"
    (doseq [sc [(s/scale :bands {:domain [:a :b]})
                (s/scale :ordinal {:domain [:a :b] :range [1 2]})
                (s/scale :quantile {:domain [1 2 3 4 5]})
                (s/scale :quantize {:domain [0.13 0.97] :range 3})
                (s/scale :threshold {:domain [0.2 10] :range [1 2 3]})
                (s/scale :histogram {:domain [1 2 3 4 5 6 7 7 8 9]})
                (s/scale :constant {:range 1})]]
      (is (identical? sc (s/nice sc)) (s/kind sc))))
  (testing "explicit ticks, degenerate domain"
    (let [sc (s/scale :linear {:domain [0.13 0.97] :ticks [0.2 0.5]})] (is (identical? sc (s/nice sc))))
    (let [sc (s/scale :linear {:domain [5 5]})] (is (identical? sc (s/nice sc))))
    (let [t (LocalDateTime/of 2012 5 5 10 10)
          sc (s/scale :datetime {:domain [t t]})] (is (identical? sc (s/nice sc)))))
  (testing "no convergence (one tick)"
    (let [sc (s/scale :linear {:domain [-10.62833214301842 7347.485977722982] :ticks 1})]
      (is (identical? sc (s/nice sc))))))

(deftest nice-keeps-attributes-test
  (let [fmt (fn [_] "X")
        sc (s/scale :log {:domain [3 7000] :base 2 :ticks 4 :ticks-method :pretty :ticks-params {:min-n 1} :formatter fmt :range [10 20]})
        niced (s/nice sc)]
    (is (= (dissoc (s/->map sc) :domain) (dissoc (s/->map niced) :domain)))
    (is (not= (s/domain sc) (s/domain niced)))))
