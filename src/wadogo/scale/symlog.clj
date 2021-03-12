;; https://www.researchgate.net/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data

(ns wadogo.scale.symlog
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale ->ScaleType strip-keys]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- forward-e ^double [^double x] (* (m/sgn x) (m/log1p (m/abs x))))
(defn- inverse-e ^double [^double y] (* (m/sgn y) (m/expm1 (m/abs y))))

(def ^:private ^:const ^double c10 (/ (m/ln 10.0)))
(defn- forward-10 ^double [^double x] (* (m/sgn x) (m/log10 (inc (m/abs (/ x c10))))))
(defn- inverse-10 ^double [^double y] (* (m/sgn y) c10 (dec (m/pow 10.0 (m/abs y)))))

(def ^:private ^:const ^double c2 (/ (m/ln 2.0)))
(defn- forward-2 ^double [^double x] (* (m/sgn x) (m/log2 (inc (m/abs (/ x c2))))))
(defn- inverse-2 ^double [^double y] (* (m/sgn y) c2 (dec (m/pow 2.0 (m/abs y)))))

(defn- make-forward
  ^double [^double base ^double C]
  (let [lbr (/ (m/log base))]
    (fn ^double [^double x]
      (* (m/sgn x)
         (m/log (inc (m/abs (/ x C))))
         lbr))))

(defn- make-inverse
  ^double [^double base ^double C]
  (fn ^double [^double y]
    (* (m/sgn y) C
       (dec (m/pow base (m/abs y))))))

(defn- symlog-forward
  [forward norm]
  (fn ^double [^double v]
    (norm (forward v))))

(defn- symlog-inverse
  [inverse norm]
  (fn ^double [^double v]
    (inverse (norm v))))

;;

(def ^:private symlog-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]
   :base 10.0})

(defn- base->forward-inverse
  [^double base ^double C]
  (cond
    (== base 10.0) [forward-10 inverse-10]
    (== base m/E) [forward-e inverse-e]
    (== base 2.0) [forward-2 inverse-2]
    (m/one? base) [identity identity]
    :else [(make-forward base C)
           (make-inverse base C)]))

(defmethod scale :symlog
  ([_] (scale :symlog {}))
  ([_ params]
   (let [params (merge symlog-params params)
         base (:base params)
         C (get params :C (/ (m/ln base)))
         [forward inverse] (base->forward-inverse base C)
         [^double dstart ^double dend] (:domain params)
         [rstart rend] (:range params)
         sls (forward dstart)
         sle (forward dend)]
     (->ScaleType :symlog (:domain params) (:range params)
                  (symlog-forward forward (m/make-norm sls sle rstart rend))
                  (symlog-inverse inverse (m/make-norm rstart rend sls sle))
                  (assoc (strip-keys params) :C C)))))
