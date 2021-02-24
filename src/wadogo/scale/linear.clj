(ns wadogo.scale.linear
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale reify-scale]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private linear-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]})

(defn make-norm-with-equal
  [^double dstart ^double dend ^double rstart ^double rend]
  (if (== dstart dend)
    (let [half (m/mlerp rstart rend 0.5)]
      (fn [^double v]
        (cond
          (< v dstart) rstart
          (> v dstart) rend
          :else half)))
    (m/make-norm dstart dend rstart rend)))

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([_ params]
   (let [params (merge linear-params params)
         [dstart dend] (:domain params)
         [rstart rend] (:range params)]
     (reify-scale 
      (make-norm-with-equal dstart dend rstart rend)
      (make-norm-with-equal rstart rend dstart dend)
      (assoc params :kind :linear)))))
