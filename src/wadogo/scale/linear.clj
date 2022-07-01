(ns wadogo.scale.linear
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [make-norm ->extent]]))

(set! *unchecked-math* :warn-on-boxed)

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))]
     (->ScaleType :linear [dstart dend] [rstart rend] (:ticks params) (:formatter params)
                  (make-norm dstart dend rstart rend)
                  (make-norm rstart rend dstart dend)
                  (strip-keys params)))))
