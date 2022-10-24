(ns wadogo.scale.ordinal
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [ensure-seq-content]]))

(defmethod scale :ordinal
  ([_] (scale :ordinal {}))
  ([s params]
   (let [{:keys [domain range sort?] :as params} (merge-params s params)
         ndomain (as-> domain domain
                   (distinct domain)
                   (ensure-seq-content domain range)
                   (if sort? (sort domain) domain))
         nrange (ensure-seq-content range ndomain)]
     (->ScaleType :ordinal ndomain nrange (:ticks params) (:formatter params)
                  (zipmap ndomain nrange)
                  (zipmap nrange ndomain)
                  (strip-keys params)))))
