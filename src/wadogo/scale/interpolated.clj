(ns wadogo.scale.interpolated
  (:require [fastmath.interpolation :refer [interpolators-1d-list]]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys]]))

(def ^:private interpolators
  {:cubic :cubic-spline
   :kriging :kriging-spline
   :linear :linear-smile})

(def default-params
  {:domain [0.0 0.5 1.0]
   :range [0.0 0.5 1.0]
   :interpolator :linear
   :interpolator-params nil})

(defmethod scale :interpolated
  ([_ ] (scale :interpolated {}))
  ([_ params]
   (let [params (merge default-params params)
         pairs (map vector (:domain params) (:range params))
         interpolator-key (get interpolators (:interpolator params) (:interpolator params))
         interpolator (apply partial (interpolators-1d-list interpolator-key) (:interpolator-params params))
         fpairs (sort-by first pairs)
         forward (interpolator (map first fpairs)
                               (map second fpairs))
         ipairs (sort-by second pairs)
         inverse (interpolator (map second ipairs)
                               (map first ipairs))]
     (->ScaleType :interpolated (:domain params) (:range params) (:ticks params) (:formatter params)
                  forward
                  inverse
                  (strip-keys params)))))
