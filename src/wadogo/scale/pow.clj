(ns wadogo.scale.pow
  (:require [fastmath.core :as m]
            
            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [->extent build-forward build-inverse]]))

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

(defmethod scale :pow
  ([_] (scale :pow {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))
         [pf pi] (map symmetric (pow-pairs (:exponent params)))
         pstart (pf dstart)
         pend (pf dend)]
     (->ScaleType :pow [dstart dend] [rstart rend] (:ticks params) (:formatter params)
                  (build-forward pf (m/make-norm pstart pend rstart rend))
                  (build-inverse pi (m/make-norm rstart rend pstart pend))
                  (strip-keys params)))))
