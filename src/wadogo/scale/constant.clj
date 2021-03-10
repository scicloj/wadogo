(ns wadogo.scale.constant
  (:require [wadogo.common :refer [scale ->ScaleType]]))

(defmethod scale :constant
  ([_] (scale :constant {}))
  ([_ params]
   (->ScaleType :constant (:domain params) (:range params)
                (constantly (:range params))
                (constantly (:domain params))
                nil)))
