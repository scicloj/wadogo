(ns wadogo.scale.threshold
  "`:threshold` kind: numbers mapped to range values by the intervals between thresholds (d3 `scaleThreshold`).

  Attributes:

  - `:domain` (default `[]`): thresholds in ascending order (repeated values give empty intervals); `n` thresholds give `n+1` intervals, the outer ones unbounded.
  - `:range`: the range values, one per interval, their count, or `:intervals` (the interval pairs `[dstart dend]` as values, formatted as ranges by [[format]]). The default is the interval positions `0..n`.
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input.

  A value equal to a threshold belongs to the upper interval. With a second truthy argument the scale returns the interval map: `:dstart`, `:dend` (infinite for the outer intervals), `:id` (position) and `:value`. The inverse maps a range value to its interval map (the first one when a value repeats). Ticks are the range values; `nice` returns the scale unchanged.

  Throws `ex-info` with `{:domain domain}` when the thresholds aren't in ascending order."
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params interval-forward]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map]]))

(defn- append-infs
  "Thresholds with `-Inf` before and `+Inf` after."
  [domain]
  (-> domain seq (conj ##-Inf) vec (conj ##Inf)))

(defmethod scale :threshold
  ([_] (scale :threshold {}))
  ([s params]
   (let [params (merge-params s params)
         domain (:domain params)
         _ (when-not (or (empty? domain) (apply <= domain))
             (throw (ex-info "Threshold scale domain should be sorted in ascending order." {:domain domain})))
         steps (append-infs domain)
         step-fn (interval-steps-before (rest steps))
         pairs (vec (partition 2 1 steps))
         range-params (or (:range params) (range (count pairs)))
         rv (if (= :intervals range-params)
              pairs
              (-> range-params build-seq second vec))
         values (mapv (fn [[x1 x2] id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :value (get rv id)}) pairs (range))]
     (->ScaleType :threshold domain rv (:ticks params) (:formatter params)
                  (interval-forward step-fn values (:unknown params))
                  (values->reversed-map values :value)
                  (strip-keys params)))))
