(ns wadogo.scale.radial
  "`:radial` kind: linear mapping of a continuous numerical domain to the squared range, so that the range values are radii of areas proportional to the domain values (d3 `scaleRadial`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`), `:range` (default `[0.0 1.0]`): as for the `:linear` kind.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind (rounding is applied to the radius).

  Forward is `sign(y) * sqrt(|y|)` of the linear mapping of the domain to the signed squares of the range ends; inverse is its inverse. Ticks and `nice` are linear in the domain. A degenerate domain behaves as for the `:linear` kind."
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [->extent make-norm]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- signed-square ^double [^double x] (* x (m/abs x)))
(defn- signed-sqrt ^double [^double x] (* (m/sgn x) (m/sqrt (m/abs x))))

(defmethod scale :radial
  ([_] (scale :radial {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->extent (:domain params))
         [^double rstart ^double rend] (->extent (:range params))
         sstart (signed-square rstart)
         send (signed-square rend)
         fnorm (make-norm dstart dend sstart send)
         inorm (make-norm sstart send dstart dend)]
     (continuous-scale :radial [dstart dend] [rstart rend] params
                       (fn ^double [^double v] (signed-sqrt (fnorm v)))
                       (fn ^double [^double v] (inorm (signed-square v)))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
