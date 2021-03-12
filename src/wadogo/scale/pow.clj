(ns wadogo.scale.pow
  (:require [fastmath.core :as m]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- symmetric
  [f]
  (fn ^double [^double x]
    (if (neg? x) (- ^double (f (- x))) (f x))))

(defn- cbrt ^double [^double x] (m/cbrt x))
(defn- sqrt ^double [^double x] (m/sqrt x))

(defn- pow-pairs
  [^double exponent]
  (cond
    (== exponent m/THIRD) [cbrt m/cb]
    (== exponent 0.5) [sqrt m/sq]
    (m/one? exponent) [identity identity]
    (== exponent 2.0) [m/sq sqrt]
    (== exponent 3.0) [m/cb cbrt]
    :else (let [rexponent (/ exponent)]
            [(fn ^double [^double x] (m/pow x exponent))
             (fn ^double [^double x] (m/pow x rexponent))])))

(defn- pow-forward
  [pf norm]
  (fn ^double [^double x]
    (norm (pf x))))

(defn- pow-inverse
  [pi norm]
  (fn ^double [^double x]
    (pi (norm x))))

(def ^:private pow-params
  {:domain [0.0 1.0]
   :range [0.0 1.0]
   :exponent 0.5})

(defmethod scale :pow
  ([_] (scale :pow {}))
  ([_ params]
   (let [params (merge pow-params params)
         [dstart dend] (:domain params)
         [rstart rend] (:range params)
         [pf pi] (map symmetric (pow-pairs (:exponent params)))
         pstart (pf dstart)
         pend (pf dend)]
     (->ScaleType :pow (:domain params) (:range params) (:ticks params) (:fmt params)
                  (pow-forward pf (m/make-norm pstart pend rstart rend))
                  (pow-inverse pi (m/make-norm rstart rend pstart pend))
                  (strip-keys params)))))
