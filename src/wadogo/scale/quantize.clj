(ns wadogo.scale.quantize
  (:require [fastmath.core :as m]

            [wadogo.common :refer [scale ->ScaleType strip-keys]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map]]
            [fastmath.stats :as stats]))

(def ^:private quantize-params
  {:domain [0.0 1.0]
   :range [0]})

(defmethod scale :quantize
  ([_] (scale :quantize {}))
  ([_ params]
   (let [params (merge quantize-params params)
         [^long n r] (build-seq (:range params))
         rv (vec r)
         [mn mx] (stats/extent (:domain params))
         steps (m/slice-range mn mx (inc n))
         step-fn (interval-steps-before (rest steps))
         values (mapv (fn [[x1 x2] id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :value (rv id)}) (partition 2 1 steps) (range n))
         forward (comp values step-fn)]
     (->ScaleType :quantize [mn mx] rv (:ticks params) (:fmt params)
                  (fn local-forward
                    ([^double v interval?]
                     (let [res (forward v)]
                       (if interval? res (:value res))))
                    ([^double v] (local-forward v false)))
                  (values->reversed-map values :value)
                  (assoc (strip-keys params) :thresholds (rest (butlast steps)))))))



