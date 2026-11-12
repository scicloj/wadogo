(ns wadogo.scale.constant
  "`:constant` kind: every value mapped to the range.

  Attributes:

  - `:domain` (default `nil`): returned by the inverse for every value.
  - `:range` (default `nil`): returned by the scale for every value, `nil` and `NaN` included (`:unknown` isn't used).

  Ticks are the domain values (all of `:domain` when it's a sequence); `nice` returns the scale unchanged."
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys]]))

(defmethod scale :constant
  ([_] (scale :constant {}))
  ([_ params]
   (->ScaleType :constant (:domain params) (:range params) (:ticks params) (:formatter params)
                (constantly (:range params))
                (constantly (:domain params))
                (strip-keys params))))
