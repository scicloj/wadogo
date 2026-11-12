(ns wadogo.scale.identity
  "`:identity` kind: numbers mapped to themselves, the domain equals the range (d3 `scaleIdentity`).

  Attributes:

  - `:domain` (default `[0.0 1.0]`): two numbers, also used as the range; a longer sequence is reduced to its minimum and maximum. When `:domain` is missing, `:range` is used. `:range` is otherwise ignored, so change the scale with `with-domain`.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind.

  Forward and inverse return the value as a double; values outside the domain are returned unchanged unless clamped. Ticks and `nice` are those of the `:linear` kind; `nice` changes the range together with the domain."
  (:require [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [->extent]]))

(defmethod scale :identity
  ([_] (scale :identity {}))
  ([s params]
   (let [domain (or (:domain params) (:range params))
         params (merge-params s (cond-> params domain (assoc :domain domain)))
         [dstart dend] (->extent (:domain params))]
     (continuous-scale :identity [dstart dend] [dstart dend] params
                       double double
                       (strip-keys params)))))
