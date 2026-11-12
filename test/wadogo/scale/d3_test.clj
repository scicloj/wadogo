(ns wadogo.scale.d3-test
  "Forward and inverse of the kinds ported from d3-scale against d3-scale 4.0.2 (`utils/d3/d3_scales.mjs`)."
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.core :as m]
            [wadogo.scale :as s]
            [wadogo.oracle :refer [load-oracle]])
  (:import [java.time LocalDateTime Duration]))

(def cases (load-oracle "d3_scales.edn"))

(defn- ->ldt [x] (if (string? x) (LocalDateTime/parse x) x))

(defn- prepare-attrs
  [{:keys [attrs time?]}]
  (cond-> attrs
    time? (update :domain #(mapv ->ldt %))))

(defn- same?
  "Equal values; numbers relatively close (1e-9, absolute 1e-9 around zero), datetimes within 1 ms."
  [expected actual]
  (cond
    (and (number? expected) (m/nan? (double expected))) (and (number? actual) (m/nan? (double actual)))
    (and (number? expected) (number? actual)) (let [e (double expected) a (double actual)]
                                                (or (== e a)
                                                    (<= (m/abs (- e a)) (max 1.0e-9 (* 1.0e-9 (max (m/abs e) (m/abs a)))))))
    (and (instance? LocalDateTime expected) (instance? LocalDateTime actual))
    (<= (m/abs (.toMillis (Duration/between ^LocalDateTime expected ^LocalDateTime actual))) 1)
    :else (= expected actual)))

(defn- forward-value
  [sc {:keys [accessor]} x]
  (case accessor
    :lower (when-let [{:keys [rstart rend]} (sc x true)] (min rstart rend))
    (sc x)))

(defn- boundary-ulp-difference?
  "`x` is a d3 quantile differing from the wadogo quantile at the same position (fastmath and d3 interpolate in different order, relative error ~1e-15)."
  [sc {:keys [quantiles]} x]
  (some (fn [[q [wq]]] (and (= q x) (not= q wq))) (map vector quantiles (s/data sc :quantiles))))

(defn- check-case
  [{:keys [id kind forward inverse time?] :as c}]
  (let [sc (s/scale kind (prepare-attrs c))
        conv (if time? ->ldt identity)]
    (testing id
      (doseq [[x y] forward
              :when (not (boundary-ulp-difference? sc c x))]
        (is (same? y (forward-value sc c (conv x))) (str id " forward " x)))
      (when-let [qs (:quantiles c)]
        (is (every? true? (map (fn [q [wq]] (same? q wq)) qs (s/data sc :quantiles))) (str id " quantiles")))
      (doseq [[y x] inverse]
        (is (same? (conv x) (s/inverse sc y)) (str id " inverse " y)))
      (when-let [bw (:bandwidth c)]
        (is (same? bw (s/data sc :bandwidth)) (str id " bandwidth")))
      (when-let [st (:step c)]
        (is (same? st (s/data sc :step)) (str id " step"))))))

(defn- check-kind
  [kind]
  (let [cs (filter #(= kind (:kind %)) cases)]
    (is (>= (count cs) 10) (str "number of " kind " cases"))
    (run! check-case cs)))

(deftest linear-d3-test (check-kind :linear))
(deftest identity-d3-test (check-kind :identity))
(deftest log-d3-test (check-kind :log))
(deftest pow-d3-test (check-kind :pow))
(deftest symlog-d3-test (check-kind :symlog))
(deftest radial-d3-test (check-kind :radial))
(deftest sequential-d3-test (check-kind :sequential))
(deftest diverging-d3-test (check-kind :diverging))
(deftest quantize-d3-test (check-kind :quantize))
(deftest quantile-d3-test (check-kind :quantile))
(deftest threshold-d3-test (check-kind :threshold))
(deftest bands-d3-test (check-kind :bands))
(deftest point-d3-test (check-kind :point))
(deftest ordinal-d3-test (check-kind :ordinal))
(deftest datetime-d3-test (check-kind :datetime))
