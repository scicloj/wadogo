(ns wadogo.scale.ordinal
  (:require [wadogo.common :refer [scale reify-scale ensure-seq-content]]))

(defmethod scale :ordinal
  ([_] (scale :ordinal {}))
  ([_ {:keys [domain range]}]
   (let [nrange (ensure-seq-content range domain )
         ndomain (ensure-seq-content domain range)]
     (reify-scale
      (zipmap ndomain nrange)
      (zipmap nrange ndomain)
      {:domain ndomain :range nrange :kind :ordinal}))))
