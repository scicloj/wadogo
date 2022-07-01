;; https://www.researchgate.net/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data

(ns wadogo.scale.symlog
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params log-params]]
            [wadogo.utils :refer [->extent]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- forward-e ^double [^double C ^double x] (* (m/sgn x) (m/log1p (m/abs (/ x C)))))
(defn- inverse-e ^double [^double C ^double y] (* (m/sgn y) C (m/expm1 (m/abs y))))

(defn- forward-10 ^double [^double C ^double x] (* (m/sgn x) (m/log10 (inc (m/abs (/ x C))))))
(defn- inverse-10 ^double [^double C ^double y] (* (m/sgn y) C (dec (m/pow 10.0 (m/abs y)))))

(defn- forward-2 ^double [^double C ^double x] (* (m/sgn x) (m/log2 (inc (m/abs (/ x C))))))
(defn- inverse-2 ^double [^double C ^double y] (* (m/sgn y) C (dec (m/pow 2.0 (m/abs y)))))

(defn- make-forward
  [^double base]
  (let [lbr (/ (m/log base))]
    (fn ^double [^double C ^double x]
      (* (m/sgn x)
         (m/log (inc (m/abs (/ x C))))
         lbr))))

(defn- make-inverse
  [^double base]
  (fn ^double [^double C ^double y]
    (* (m/sgn y) C
       (dec (m/pow base (m/abs y))))))

(defn- symlog-forward
  [^double C forward norm]
  (fn ^double [^double v]
    (norm (forward C v))))

(defn- symlog-inverse
  [^double C inverse norm]
  (fn ^double [^double v]
    (inverse C (norm v))))

;;

(defn- symlog-identity ^double [^double _C ^double v] v)

(defn- base->forward-inverse
  [^double base]
  (cond
    (== base 10.0) [forward-10 inverse-10]
    (== base m/E) [forward-e inverse-e]
    (== base 2.0) [forward-2 inverse-2]
    (m/one? base) [symlog-identity symlog-identity]
    :else [(make-forward base)
           (make-inverse base)]))

(defmethod scale :symlog
  ([_] (scale :symlog {}))
  ([s params]
   (let [params (merge-params s (log-params params))
         base (:base params)
         C (or (:C params) (/ (m/ln base)))
         [forward inverse] (base->forward-inverse base)
         [^double dstart ^double dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))
         sls (forward C dstart)
         sle (forward C dend)]
     (->ScaleType :symlog [dstart dend] [rstart rend] (:ticks params) (:formatter params)
                  (symlog-forward C forward (m/make-norm sls sle rstart rend))
                  (symlog-inverse C inverse (m/make-norm rstart rend sls sle))
                  (assoc (strip-keys params) :C C)))))
