(ns wadogo.scale.constant
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys]]))

(defmethod scale :constant
  ([_] (scale :constant {}))
  ([_ params]
   (->ScaleType :constant (:domain params) (:range params) (:ticks params) (:formatter params)
                (constantly (:range params))
                (constantly (:domain params))
                (strip-keys params))))
