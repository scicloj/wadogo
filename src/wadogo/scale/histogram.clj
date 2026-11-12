(ns wadogo.scale.histogram
  "`:histogram` kind: numbers mapped to range values by the bins of a histogram of a data sample.

  Attributes:

  - `:domain` (required): the data sample, a sequence of numbers (`nil` values are removed).
  - `:range` (default `:freedman-diaconis`): a bin estimation method of `fastmath.stats/estimate-bins` (as `:sturges`, `:sqrt`, `:rice`, `:doane`, `:scott`, `:freedman-diaconis`; `:default` and `nil` mean `:freedman-diaconis`) with the bin positions `0..n-1` as values, a bin count `n`, or the range values (one per bin).
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input.

  A value equal to a bin border belongs to the upper bin; values below the sample minimum belong to the first bin and values above the maximum to the last one. With a second truthy argument the scale returns the bin map: `:dstart`, `:dend`, `:id` (position), `:count` (sample values inside) and `:value`.

  The inverse maps a range value to its bin map. The `fastmath.stats/histogram` bins are stored under `:bins` in [[data]]. Ticks are the range values; `nice` returns the scale unchanged."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params interval-forward]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map]]))

(defn- ->bins
  "`[n values]` for the `:range` attribute `r` and the sample `xs`."
  [xs r]
  ;; older fastmath `estimate-bins` treated any unknown keyword (incl. `:default`) as Freedman-Diaconis
  (let [r (if (or (nil? r) (= r :default)) :freedman-diaconis r)]
    (if (keyword? r)
      (let [cnt (stats/estimate-bins xs r)]
        [cnt (range cnt)])
      (build-seq r))))

(defmethod scale :histogram
  ([_] (scale :histogram {}))
  ([s params]
   (assert (seq (:domain params)) "Domain can't be empty, please provide any data as a sequence of numbers")
   (let [params (merge-params s params)
         xs (m/seq->double-array (remove nil? (:domain params)))
         [n r] (->bins xs (:range params))
         rv (vec r)
         histogram (stats/histogram xs n)
         steps (conj (mapv first (:bins histogram)) (:max histogram))
         counts (map second (:bins histogram))
         step-fn (interval-steps-before (rest steps))
         values (mapv (fn [[x1 x2] cnt id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :count cnt
                         :value (rv id)}) (partition 2 1 steps) counts (range n))]
     (->ScaleType :histogram xs rv (:ticks params) (:formatter params)
                  (interval-forward step-fn values (:unknown params))
                  (values->reversed-map values :value)
                  (assoc (strip-keys params) :bins (:bins histogram))))))
