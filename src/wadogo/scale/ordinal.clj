(ns wadogo.scale.ordinal
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys]]
            [wadogo.utils :refer [ensure-seq-content]]))

(defmethod scale :ordinal
  ([_] (scale :ordinal {}))
  ([_ {:keys [domain range] :as params}]
   (let [nrange (ensure-seq-content range domain)
         ndomain (ensure-seq-content domain range)]
     (->ScaleType :ordinal ndomain nrange (:ticks params) (:fmt params)
                  (zipmap ndomain nrange)
                  (zipmap nrange ndomain)
                  (strip-keys params)))))
