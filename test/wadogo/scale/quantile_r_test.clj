(ns wadogo.scale.quantile-r-test
  "Quantiles of the `:quantile` kind for every estimation strategy against R's `quantile(type = k)` (`utils/quantile/quantile_oracle.R`)."
  (:require [clojure.test :refer [deftest is]]
            [wadogo.scale :as s]
            [wadogo.oracle :refer [load-oracle close?]]))

(def cases (load-oracle "r_quantiles.edn"))

(defn- fuzzed-type-2?
  "R's type 2 treats `m*p` within 4 eps of an integer as an integer (averaging two order statistics); fastmath (Apache Commons Math `R_2`) doesn't, so cases with an integral `m*k/n` are excluded (a fastmath difference, not a wadogo one)."
  [data n type]
  (and (== 2 type)
       (some #(zero? (mod (* (count data) %) n)) (range 1 n))))

(deftest quantile-strategies-r-test
  (doseq [{:keys [data n type quantiles]} cases
          :when (not (fuzzed-type-2? data n type))
          :let [sc (s/scale :quantile {:domain data :range n :estimation-strategy (keyword (str "r" type))})
                actual (map first (s/data sc :quantiles))]]
    (is (and (= (count quantiles) (count actual))
             (every? true? (map #(close? %1 %2 1.0e-12) quantiles actual)))
        (str "type " type ", n " n ", data " data ": " (vec actual)))))

(deftest quantile-default-strategy-test
  (is (= :r7 (s/data (s/scale :quantile {:domain [1 2 3]}) :estimation-strategy))))
