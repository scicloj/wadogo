(ns wadogo.scale.linear
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [make-norm]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (:domain params)
         [rstart rend] (:range params)]
     (->ScaleType :linear (:domain params) (:range params) (:ticks params) (:formatter params)
                  (make-norm dstart dend rstart rend)
                  (make-norm rstart rend dstart dend)
                  (strip-keys params)))))
