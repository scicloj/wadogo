(ns wadogo.scale.diverging
  "`:diverging` kind: a continuous numerical domain `[low mid high]` normalised piecewise to `[0, 0.5, 1]` and passed to an interpolator function, typically a diverging color gradient (d3 `scaleDiverging`, `scaleDivergingLog`, `scaleDivergingPow`, `scaleDivergingSymlog`).

  Attributes:

  - `:domain`: three numbers (default `[-1.0 0.0 1.0]`, `[0.1 1.0 10.0]` for the `:log` transform).
  - `:interpolator`, `:transform`, `:exponent`, `:C`, `:base`, `:clamp`, `:unknown`, `:round`: as for the `:sequential` kind.

  `mid` should lie between `low` and `high` (in the transformed domain); otherwise the halves overlap and the mapping isn't monotone (d3 behaves the same). For the `:log` transform all three values must have the same sign, a `mid` of the other sign gives `NaN`. Values between `low` and `mid` are normalised to `[0, 0.5]`, values between `mid` and `high` to `[0.5, 1]`, each half linearly in the transformed domain. A degenerate half (equal ends) maps to `0.5`, as d3. The range is `[(interpolator 0) (interpolator 0.5) (interpolator 1)]` and the `:range` attribute is ignored. The inverse maps a normalised value from `[0, 1]` back to the domain.

  Ticks and `nice` use the first and last domain value (`nice` keeps `mid`).

  Throws `ex-info` as the `:sequential` kind, and with `{:domain domain}` when the domain doesn't have three values."
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale continuous-scale strip-keys]]
            [wadogo.scale.sequential :refer [transform-pair validate-interpolator transformed-params]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- half-slope
  "Slope normalising the transformed half `[a, b]` to a half of `[0, 1]`, `0.0` for a degenerate half."
  ^double [^double a ^double b]
  (if (== a b) 0.0 (/ 0.5 (- b a))))

(defmethod scale :diverging
  ([_] (scale :diverging {}))
  ([s params]
   (let [params (transformed-params s params [0.1 1.0 10.0])
         domain (vec (:domain params))
         _ (when-not (== 3 (count domain))
             (throw (ex-info "Diverging scale domain should have three values." {:domain domain})))
         interpolator (validate-interpolator (:interpolator params))
         [x0 x1 x2] (map double domain)
         [forward inverse] (transform-pair params x0 x2)
         ^double t0 (forward x0)
         ^double t1 (forward x1)
         ^double t2 (forward x2)
         k10 (half-slope t0 t1)
         k21 (half-slope t1 t2)
         sgn (if (< t1 t0) -1.0 1.0)]
     (continuous-scale :diverging [x0 x1 x2]
                       [(interpolator 0.0) (interpolator 0.5) (interpolator 1.0)] params
                       (fn [^double v]
                         (let [^double t (forward v)]
                           (interpolator (+ 0.5 (* (- t t1) (if (< (* sgn t) (* sgn t1)) k10 k21)))))) 
                       (fn ^double [^double t]
                         (let [k (if (< t 0.5) k10 k21)]
                           (inverse (if (zero? k) t1 (+ t1 (/ (- t 0.5) k))))))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
