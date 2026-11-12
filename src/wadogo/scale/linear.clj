(ns wadogo.scale.linear
  "`:linear` kind: continuous numerical domain mapped linearly to a continuous numerical range (d3 `scaleLinear`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`): two numbers in any order; a longer sequence is reduced to its minimum and maximum (use the `:interpolated` kind for piecewise mappings).
  - `:range` (default `[0.0 1.0]`): two numbers, reduced the same way.
  - `:clamp` (default `false`): when truthy, values outside the domain are constrained to it, and the inverse returns values inside the domain.
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input.
  - `:round` (default `false`): when truthy, the result is rounded half up (d3 `rangeRound`).

  Forward maps the domain ends to the range ends and extrapolates outside; inverse maps back.

  Ticks are d3 style (`:ticks-method :default`, 10 ticks) or R `pretty()` style (`:pretty`, 5 ticks); `nice` extends the domain to multiples of the tick step.

  A degenerate domain (equal ends) maps values below it to the range start, above it to the range end and the value itself to the middle of the range (d3 maps every value to the middle). A degenerate range behaves the same way in the inverse."
  (:require [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [make-norm ->extent]]))

(set! *unchecked-math* :warn-on-boxed)

(defmethod scale :linear
  ([_] (scale :linear {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->extent (:domain params))
         [rstart rend] (->extent (:range params))]
     (continuous-scale :linear [dstart dend] [rstart rend] params
                       (make-norm dstart dend rstart rend)
                       (make-norm rstart rend dstart dend)
                       (strip-keys params)))))
