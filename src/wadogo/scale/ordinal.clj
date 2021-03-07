(ns wadogo.scale.ordinal
  (:require [wadogo.common :refer [scale ->ScaleType]]
            [wadogo.utils :refer [ensure-seq-content]]))

(defmethod scale :ordinal
  ([_] (scale :ordinal {}))
  ([_ {:keys [domain range]}]
   (let [nrange (ensure-seq-content range domain )
         ndomain (ensure-seq-content domain range)]
     (->ScaleType :ordinal ndomain nrange
                  (zipmap ndomain nrange)
                  (zipmap nrange ndomain)
                  nil))))
