(ns wadogo.scale.log
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale ->ScaleType strip-keys]]
            [fastmath.stats :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- log-forward
  [negative? norm]
  (if negative?
    (fn ^double [^double v] (norm (m/log (- v))))
    (fn ^double [^double v] (norm (m/log v)))))

(defn- log-inverse
  [negative? norm]
  (if negative?
    (fn ^double [^double v] (- (m/exp (norm v))))
    (fn ^double [^double v] (m/exp (norm v)))))

(def default-params
  {:domain [1.0 10.0]
   :range [0.0 1.0]
   :base 10.0})

(defmethod scale :log
  ([_] (scale :log {}))
  ([_ params]
   (let [params (merge default-params params)
         [^double dstart ^double dend] (stats/extent (:domain params))
         [rstart rend] (stats/extent (:range params))
         n? (neg? dstart)
         ls (m/log (if n? (- dstart) dstart))
         le (m/log (if n? (- dend) dend))]
     (->ScaleType :log (:domain params) (:range params)
                  (log-forward n? (m/make-norm ls le rstart rend))
                  (log-inverse n? (m/make-norm rstart rend ls le))
                  (strip-keys params)))))
