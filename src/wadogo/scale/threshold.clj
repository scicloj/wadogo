(ns wadogo.scale.threshold
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map]]))

(defn- fix-domain
  [domain]
  (-> domain seq (conj ##-Inf) vec (conj ##Inf)))

(defmethod scale :threshold
  ([_] (scale :threshold {}))
  ([s params]
   (let [params (merge-params s params)
         [^long n r] (build-seq (:range params))
         rv (vec r)
         steps (fix-domain (:domain params))
         step-fn (interval-steps-before (rest steps))
         values (mapv (fn [[x1 x2] id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :value (rv id)}) (partition 2 1 steps) (range n))
         forward (comp values step-fn)]
     (->ScaleType :threshold (:domain params) rv (:ticks params) (:formatter params)
                  (fn local-forward
                    ([^double v interval?]
                     (let [res (forward v)]
                       (if interval? res (:value res))))
                    ([^double v] (local-forward v false)))
                  (values->reversed-map values :value)
                  (strip-keys params)))))

