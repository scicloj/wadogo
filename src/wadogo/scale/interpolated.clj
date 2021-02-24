(ns wadogo.scale.interpolated
  (:require [fastmath.interpolation :refer [interpolators-1d-list]]
            
            [wadogo.common :refer [scale reify-scale]]))


(def ^:private interpolated-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]
   :interpolator :linear-smile
   :interpolator-params nil})

(defmethod scale :interpolated
  ([_ ] (scale :interpolated {}))
  ([_ params]
   (let [params (merge interpolated-params params)
         pairs (map vector (:domain params) (:range params))
         interpolator (apply partial (interpolators-1d-list (:interpolator params)) (:interpolator-params nil))
         fpairs (sort-by first pairs)
         forward (interpolator (map first fpairs)
                               (map second fpairs))
         ipairs (sort-by second pairs)
         inverse (interpolator (map second ipairs)
                               (map first ipairs))]
     (reify-scale
      forward
      inverse
      (assoc params :kind :interpolated)))))

