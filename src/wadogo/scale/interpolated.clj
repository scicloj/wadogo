(ns wadogo.scale.interpolated
  (:require [fastmath.interpolation :as i]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]))

(defmethod scale :interpolated
  ([_ ] (scale :interpolated {}))
  ([s params]
   (let [params (merge-params s params)
         pairs (map vector (:domain params) (:range params))
         fpairs (sort-by first pairs)
         interpolator (:interpolator params :linear)
         interpolator-params (:interpolator-params params)
         forward (if (fn? interpolator)
                   (apply interpolator
                          (map first fpairs) (map second fpairs)
                          interpolator-params)
                   (apply i/interpolation interpolator
                          (map first fpairs) (map second fpairs)
                          interpolator-params))
         ipairs (sort-by second pairs)
         inverse (if (fn? interpolator)
                   (apply interpolator
                          (map first fpairs) (map second fpairs)
                          interpolator-params)
                   (apply i/interpolation interpolator
                          (map second ipairs)
                          (map first ipairs)
                          interpolator-params))]
     (->ScaleType :interpolated (:domain params) (:range params) (:ticks params) (:formatter params)
                  forward
                  inverse
                  (strip-keys params)))))
