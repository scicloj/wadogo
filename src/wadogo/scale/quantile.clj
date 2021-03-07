(ns wadogo.scale.quantile
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.common :refer [scale ->ScaleType]]
            [wadogo.utils :refer [interval-steps-before strip-keys]]))

(def ^:private quantile-params
  {:quantiles 10
   :estimation-strategy :legacy
   :align 0.5})

(defmethod scale :quantile
  ([_] (scale :quantile {}))
  ([_ params]
   (let [params (merge quantile-params params)
         xs (m/seq->double-array (:domain params))
         quantiles (:quantiles params)
         align (m/constrain ^double (:align params) 0.0 1.0)
         [start end] (stats/extent xs)
         quantiles (if (sequential? quantiles)
                     quantiles
                     (rest (m/slice-range (inc (long quantiles)))))
         steps (stats/quantiles xs quantiles (:estimation-strategy params))
         steps-corr (conj (seq steps) start)
         r (mapv (fn [id [^double x1 ^double x2] q]
                   {:start x1
                    :end x2
                    :point (m/mlerp x1 x2 align)
                    :id id
                    :quantile q}) (range) (partition 2 1 steps-corr) quantiles)
         forward (comp r (interval-steps-before steps))]
     (->ScaleType :quantile xs [start end]
                  (fn [^double v]
                    (when (<= start v end)
                      (forward v)))
                  (constantly nil)
                  (assoc (strip-keys params) :quantiles quantiles)))))

#_((scale :quantile {:domain d
                     :quantiles [0.25 0.5 0.75 1]}) 7.9)

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
