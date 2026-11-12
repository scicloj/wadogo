(ns wadogo.scale.pow
  "`:pow` kind: power mapping of a continuous numerical domain to a continuous numerical range (d3 `scalePow`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`), `:range` (default `[0.0 1.0]`): as for the `:linear` kind.
  - `:exponent` (default `0.5`, square root).
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind.

  Forward is linear in `sign(x) * |x|^exponent`, so negative values are mirrored. A negative exponent gives a decreasing transform, normalised back to the range ends. Exponent `0.0` maps every value, zero included, to `1.0`, so the transformed domain is degenerate and every value maps to the middle of the range. Ticks and `nice` are linear in the domain. A degenerate domain behaves as for the `:linear` kind."
  (:require [fastmath.core :as m]

            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [->extent make-norm]]))

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

(defn ^:no-doc pow-transform
  "Internal. `[forward inverse]` power transform `sign(x) * |x|^exponent` and its inverse."
  [exponent]
  (mapv symmetric (pow-pairs exponent)))

(defmethod scale :pow
  ([_] (scale :pow {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))
         [pf pi] (pow-transform (:exponent params))
         pstart (pf dstart)
         pend (pf dend)
         fnorm (make-norm pstart pend rstart rend)
         inorm (make-norm rstart rend pstart pend)]
     (continuous-scale :pow [dstart dend] [rstart rend] params
                       (fn ^double [^double v] (fnorm (pf v)))
                       (fn ^double [^double v] (pi (inorm v)))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
