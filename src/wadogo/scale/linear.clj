(ns wadogo.scale.linear
  (:require [wadogo.common :refer [scale ticks ->ScaleType strip-keys]]
            [wadogo.utils :refer [make-norm]]
            [fastmath.stats :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]})

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([_ params]
   (let [params (merge default-params params)
         [dstart dend] (stats/extent (:domain params))
         [rstart rend] (stats/extent (:range params))]
     (->ScaleType :linear (:domain params) (:range params) (:ticks params) (:fmt params)
                  (make-norm dstart dend rstart rend)
                  (make-norm rstart rend dstart dend)
                  (strip-keys params)))))
