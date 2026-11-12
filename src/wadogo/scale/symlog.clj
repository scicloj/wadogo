(ns wadogo.scale.symlog
  "`:symlog` kind: bi-symmetric logarithmic mapping of a continuous numerical domain (any sign, zero included) to a continuous numerical range (d3 `scaleSymlog`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`), `:range` (default `[0.0 1.0]`): as for the `:linear` kind.
  - `:C` (default `1.0`, also for `nil`): the positive constant of the transform `sign(x) * log(1 + |x|/C)`; values with `|x|` much smaller than `C` are mapped almost linearly. `0.0` gives `NaN` for every value, a negative `C` isn't meaningful.
  - `:base`: deprecated and ignored (the base of the logarithm cancels in the mapping); kept for compatibility.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind.

  Ticks and `nice` are linear in the domain. A degenerate domain behaves as for the `:linear` kind.

  See: Webber, A bi-symmetric log transformation for wide-range data, Measurement Science and Technology (2013)."
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [->extent make-norm]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ^:no-doc symlog-transform
  "Internal. `[forward inverse]` symlog transform `sign(x) * log1p(|x|/C)` and its inverse, as d3."
  [^double C]
  [(fn ^double [^double x] (* (m/sgn x) (m/log1p (m/abs (/ x C)))))
   (fn ^double [^double y] (* (m/sgn y) C (m/expm1 (m/abs y))))])

(defmethod scale :symlog
  ([_] (scale :symlog {}))
  ([s params]
   (let [params (merge-params s params)
         C (double (or (:C params) 1.0))
         [forward inverse] (symlog-transform C)
         [dstart dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))
         sls (forward dstart)
         sle (forward dend)
         fnorm (make-norm sls sle rstart rend)
         inorm (make-norm rstart rend sls sle)]
     (continuous-scale :symlog [dstart dend] [rstart rend] params
                       (fn ^double [^double v] (fnorm (forward v)))
                       (fn ^double [^double v] (inverse (inorm v)))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
