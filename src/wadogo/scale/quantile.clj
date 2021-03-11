(ns wadogo.scale.quantile
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.common :refer [scale ->ScaleType]]
            [wadogo.utils :refer [interval-steps-before strip-keys]]))

(def ^:private quantile-params
  {:domain [0]
   :range 4
   :estimation-strategy :legacy})

(defn- build-quantiles?
  [r]
  (if (sequential? r)
    (if (every? #(and (number? %)
                      (pos? %)
                      (<= % 1.0)) r)
      [true r]
      [false (rest (m/slice-range (inc (count r))))])
    [true (rest (m/slice-range (inc (long r))))]))

(defmethod scale :quantile
  ([_] (scale :quantile {}))
  ([_ params]
   (assert (seq (:domain params)) "Domain can't be empty, please provide any data as a sequence of numbers")
   (let [params (merge quantile-params params)
         xs (m/seq->double-array (remove nil? (:domain params)))
         [start end] (stats/extent xs)
         r (:range params)
         [quantiles? quantiles] (build-quantiles? r)

         steps (stats/quantiles xs quantiles (:estimation-strategy params))
         steps-corr (conj (seq steps) start)
         step-fn (interval-steps-before steps)
         ids (->> xs (map step-fn) frequencies)
         values (mapv (fn [id [^double x1 ^double x2] q]
                        {:dstart x1
                         :dend x2
                         :value (if quantiles? q (nth r id))
                         :count (ids id)
                         :quantile q}) (range) (partition 2 1 steps-corr) quantiles)
         forward (comp values step-fn)]
     (->ScaleType :quantile xs (if quantiles? quantiles r)
                  (fn local-forward
                    ([^double v interval?]
                     (let [res (when (<= start v end) (forward v))]
                       (if interval? res (:value res))))
                    ([^double v] (local-forward v false)))
                  (reduce (fn [curr m]
                            (if (curr (:value m)) curr
                                (assoc curr (:value m) m))) {} values)
                  (assoc (strip-keys params) :quantiles (butlast (map (juxt :dend :quantile) values)))))))


#_(scale :quantile {:domain d
                    :quantiles [0.25 0.5 0.75 1]})

#_(def d
    [5.1
     4.9
     4.7
     4.6
     5.0
     5.4
     4.6
     5.0
     4.4
     4.9
     5.4
     4.8
     4.8
     4.3
     5.8
     5.7
     5.4
     5.1
     5.7
     5.1
     5.4
     5.1
     4.6
     5.1
     4.8
     5.0
     5.0
     5.2
     5.2
     4.7
     4.8
     5.4
     5.2
     5.5
     4.9
     5.0
     5.5
     4.9
     4.4
     5.1
     5.0
     4.5
     4.4
     5.0
     5.1
     4.8
     5.1
     4.6
     5.3
     5.0
     7.0
     6.4
     6.9
     5.5
     6.5
     5.7
     6.3
     4.9
     6.6
     5.2
     5.0
     5.9
     6.0
     6.1
     5.6
     6.7
     5.6
     5.8
     6.2
     5.6
     5.9
     6.1
     6.3
     6.1
     6.4
     6.6
     6.8
     6.7
     6.0
     5.7
     5.5
     5.5
     5.8
     6.0
     5.4
     6.0
     6.7
     6.3
     5.6
     5.5
     5.5
     6.1
     5.8
     5.0
     5.6
     5.7
     5.7
     6.2
     5.1
     5.7
     6.3
     5.8
     7.1
     6.3
     6.5
     7.6
     4.9
     7.3
     6.7
     7.2
     6.5
     6.4
     6.8
     5.7
     5.8
     6.4
     6.5
     7.7
     7.7
     6.0
     6.9
     5.6
     7.7
     6.3
     6.7
     7.2
     6.2
     6.1
     6.4
     7.2
     7.4
     7.9
     6.4
     6.3
     6.1
     7.7
     6.3
     6.4
     6.0
     6.9
     6.7
     6.9
     5.8
     6.8
     6.7
     6.7
     6.3
     6.5
     6.2
     5.9])
