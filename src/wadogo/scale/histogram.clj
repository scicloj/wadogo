(ns wadogo.scale.histogram
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [build-seq interval-steps-before values->reversed-map]]))

;; similar to threshold but splits by bins

(defn- ->bins
  [xs r]
  (let [r (or r :default)]
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
         step-fn (interval-steps-before (rest steps))
         values (mapv (fn [[x1 x2] id]
                        {:dstart x1
                         :dend x2
                         :id id
                         :value (rv id)}) (partition 2 1 steps) (range n))
         forward (comp values step-fn)]
     (->ScaleType :histogram xs rv (:ticks params) (:formatter params)
                  (fn local-forward
                    ([^double v interval?]
                     (let [res (forward v)]
                       (if interval? res (:value res))))
                    ([^double v] (local-forward v false)))
                  (values->reversed-map values :value)
                  (assoc (strip-keys params) :bins (:bins histogram))))))
