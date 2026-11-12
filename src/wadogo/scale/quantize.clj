(ns wadogo.scale.quantize
  "`:quantize` kind: a continuous numerical domain split into equal intervals, each mapped to a range value (d3 `scaleQuantize`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`): two numbers in ascending order; a longer sequence is reduced to its minimum and maximum.
  - `:range` (default `[0]`): the range values, one per interval, or their count `n` (values `0..n-1`).
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input.

  A value equal to an interval border belongs to the upper interval; values outside the domain belong to the first or the last interval. With a second truthy argument the scale returns the interval map: `:dstart`, `:dend`, `:id` (position) and `:value`.

  The inverse maps a range value to its interval map (the first one when a value repeats). The inner borders are stored under `:thresholds` in [[data]]. Ticks are the range values; `nice` returns the scale unchanged.

  Throws `ex-info` with `{:domain domain}` for a two-value domain in descending order."
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params interval-forward]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map ->extent]]))

(defn- quantize-steps
  "`n+1` interval borders of `[x0, x1]`, the inner ones computed as d3: `((i+1) * x1 - (i-n+1) * x0) / n`."
  [x0 x1 ^long n]
  (let [x0 (double x0) x1 (double x1)]
    (concat [x0]
            (map (fn [^long i] (/ (- (* (inc i) x1) (* (- i (dec n)) x0)) n)) (range (dec n)))
            [x1])))

(defmethod scale :quantize
  ([_] (scale :quantize {}))
  ([s params]
   (let [params (merge-params s params)
         [^long n r] (build-seq (:range params))
         rv (vec r)
         [mn mx :as domain] (->extent (:domain params))
         _ (when (pos? (compare mn mx))
             (throw (ex-info "Quantize scale domain should be in ascending order." {:domain domain})))
         steps (quantize-steps mn mx n)
         step-fn (interval-steps-before (rest steps))
         values (mapv (fn [[x1 x2] id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :value (rv id)}) (partition 2 1 steps) (range n))]
     (->ScaleType :quantize [mn mx] rv (:ticks params) (:formatter params)
                  (interval-forward step-fn values (:unknown params))
                  (values->reversed-map values :value)
                  (assoc (strip-keys params) :thresholds (rest (butlast steps)))))))
