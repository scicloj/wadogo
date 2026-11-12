(ns wadogo.scale.quantile
  "`:quantile` kind: numbers mapped to range values by the quantiles of a data sample (d3 `scaleQuantile`).

  Attributes:

  - `:domain` (required): the data sample, a sequence of numbers (`nil` values are removed).
  - `:range` (default `4`): the range values (one per interval, the sample split at equally spaced probabilities), their count `n` (values are the upper probabilities `1/n, 2/n, ..., 1.0`), or an increasing sequence of probabilities from `(0, 1]` (used as the upper probabilities and as values; `1.0` is appended when missing).
  - `:estimation-strategy` (default `:r7`, as d3 and R's default): quantile estimation method of `fastmath.stats/quantiles`, as `:legacy`, `:r1` to `:r9`.
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input.

  A value equal to a quantile belongs to the upper interval; values below the sample minimum belong to the first interval and values above the maximum to the last one. With a second truthy argument the scale returns the interval map: `:dstart`, `:dend`, `:value`, `:count` (sample values inside) and `:quantile` (upper probability).

  The inverse maps a range value to its interval map (the first one when a value repeats). The inner quantiles with their probabilities are stored under `:quantiles` in [[data]]. Ticks are the range values; `nice` returns the scale unchanged."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params interval-forward]]
            [wadogo.utils :refer [interval-steps-before values->reversed-map]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- build-quantiles
  "`[values-are-quantiles? upper-probabilities]` for the `:range` attribute `r`."
  [r]
  (if (sequential? r)
    (if (every? (fn [v] (and (number? v)
                            (pos? ^double v)
                            (<= ^double v 1.0))) r)
      [true (if (< (double (last r)) 1.0) (conj (vec r) 1.0) r)] ;; ensure last element is always 1.0
      [false (rest (m/slice-range (inc (count r))))])
    [true (rest (m/slice-range (inc (long r))))]))

(defmethod scale :quantile
  ([_] (scale :quantile {}))
  ([s params]
   (assert (seq (:domain params)) "Domain can't be empty, please provide any data as a sequence of numbers")
   (let [params (merge-params s params)
         xs (m/seq->double-array (remove nil? (:domain params)))
         [^double start] (stats/extent xs)
         r (:range params)
         [quantiles? quantiles] (build-quantiles r)
         
         steps (stats/quantiles xs quantiles (:estimation-strategy params))
         steps-corr (conj (seq steps) start)
         step-fn (interval-steps-before steps)
         ids (->> xs (map step-fn) frequencies)
         values (mapv (fn [id [^double x1 ^double x2] q]
                        {:dstart x1
                         :dend x2
                         :value (if quantiles? q (nth r id))
                         :count (ids id 0)
                         :quantile q}) (range) (partition 2 1 steps-corr) quantiles)]
     (->ScaleType :quantile xs (if quantiles? quantiles r) (:ticks params) (:formatter params)
                  (interval-forward step-fn values (:unknown params))
                  (values->reversed-map values :value)
                  (assoc (strip-keys params) :quantiles (butlast (map (juxt :dend :quantile) values)))))))

(m/unuse-primitive-operators)
