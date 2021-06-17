(ns wadogo.scale.log
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params log-params]]))

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

(defmethod scale :log
  ([_] (scale :log {}))
  ([s params]
   (let [params (merge-params s (log-params params))
         [^double dstart ^double dend] (:domain params)
         [rstart rend] (:range params)
         n? (neg? dstart)
         ls (m/log (if n? (- dstart) dstart))
         le (m/log (if n? (- dend) dend))]
     (->ScaleType :log (:domain params) (:range params) (:ticks params) (:formatter params)
                  (log-forward n? (m/make-norm ls le rstart rend))
                  (log-inverse n? (m/make-norm rstart rend ls le))
                  (strip-keys params)))))
