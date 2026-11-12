(ns wadogo.scale.point
  "`:point` kind: discrete values mapped to evenly spaced points of a continuous numerical range (d3 `scalePoint`, bands of zero width).

  Attributes:

  - `:domain` (default `1`): the domain values (repeated values removed, first occurrences kept) or their count `n` (values `0..n-1`).
  - `:range` (default `[0.0 1.0]`): two numbers in any order; a longer sequence is reduced to its minimum and maximum.
  - `:padding` (default `0.0`): space before the first and after the last point as a proportion of the step (d3 `padding`), non-negative.
  - `:align-outer` (default `0.5`): how the outer space is distributed: `0.0` all after the last point, `1.0` all before the first point (d3 `align`, named as in the `:bands` kind).
  - `:round` (default `false`): when truthy, the step is floored and the first point rounded, as d3 `round`.
  - `:unknown` (default `nil`): returned for values outside the domain.

  Forward returns the point of a value; with a second truthy argument it returns the map with `:value`, `:point`, and `:rstart` and `:rend` (both equal to the point). The step between points is stored under `:step`, the maps under `:bands` in [[data]] (`:bandwidth` is `0.0`). The inverse returns the domain value of the point nearest to a number (ties to the first one), `:unknown` for numbers farther than half a step from every point.

  A single domain value is placed by `:align-outer` (the middle of the range by default). An empty domain gives no points.

  Ticks are the domain values; `nice` returns the scale unchanged."
  (:require [fastmath.core :as m]

            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params missing-value?]]
            [wadogo.utils :refer [build-seq ->extent]]
            [wadogo.scale.bands :refer [band-layout band-maps distinct-values]])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- point-inverse-fn
  "Domain value of the point nearest to a number, `unknown` when farther than `half-step`."
  [points ^double half-step unknown]
  (fn [v]
    (if (or (missing-value? v) (empty? points))
      unknown
      (let [v (double v)
            {:keys [value ^double point]} (apply min-key (fn [{:keys [^double point]}] (m/abs (- point v))) (reverse points))]
        (if (<= (m/abs (- point v)) half-step) value unknown)))))

(defmethod scale :point
  ([_] (scale :point {}))
  ([s params]
   (let [params (merge-params s params)
         [^double rstart ^double rend] (->extent (:range params))
         [^long n values] (build-seq (distinct-values (:domain params)))
         {:keys [^double padding ^double align-outer unknown]} params
         r (Vec2. rstart rend)
         ^Vec3 layout (band-layout n r (Vec3. 1.0 padding (m/constrain align-outer 0.0 1.0)) (:round params))
         lst (band-maps values r layout (repeat 0.0))
         forward (zipmap values lst)
         step (.y layout)]
     (->ScaleType :point values [rstart rend] (:ticks params) (:formatter params)
                  (fn local-forward
                    ([v] (local-forward v false))
                    ([v interval?]
                     (if-let [res (forward v)]
                       (if interval? res (:point res))
                       unknown)))
                  (point-inverse-fn lst (* 0.5 step) unknown)
                  (assoc (strip-keys params)
                         :bandwidth 0.0
                         :step step
                         :bands lst)))))

(m/unuse-primitive-operators)
