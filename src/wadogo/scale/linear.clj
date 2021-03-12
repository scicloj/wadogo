(ns wadogo.scale.linear
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys]]
            [wadogo.utils :refer [make-norm]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private linear-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]})

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([_ params]
   (let [params (merge linear-params params)
         [dstart dend] (:domain params)
         [rstart rend] (:range params)]
     (->ScaleType :linear (:domain params) (:range params) 
                  (make-norm dstart dend rstart rend)
                  (make-norm rstart rend dstart dend)
                  (strip-keys params)))))
