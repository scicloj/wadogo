(ns wadogo.scale.log
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale reify-scale deep-merge]]))

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

(def ^:private log-params
  {:domain [1.0 10.0]
   :range [0.0 1.0]
   :base 10.0})

(defmethod scale :log
  ([_] (scale :log {}))
  ([_ params]
   (let [params (deep-merge log-params params)
         [^double dstart ^double dend] (:domain params)
         [rstart rend] (:range params)
         n? (neg? dstart)
         ls (m/log (if n? (- dstart) dstart))
         le (m/log (if n? (- dend) dend))]
     (reify-scale 
      (log-forward n? (m/make-norm ls le rstart rend))
      (log-inverse n? (m/make-norm rstart rend ls le))
      (assoc params :kind :log)))))

