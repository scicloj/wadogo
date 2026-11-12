(ns wadogo.scale.bands
  "`:bands` kind: discrete values mapped to evenly distributed bands (intervals) of a continuous numerical range (d3 `scaleBand`).

  Attributes:

  - `:domain` (default `1`): the domain values (repeated values removed, first occurrences kept) or their count `n` (values `0..n-1`).
  - `:range` (default `[0.0 1.0]`): two numbers in any order; a longer sequence is reduced to its minimum and maximum.
  - `:padding-in` (default `0.0`): space between bands as a proportion of the step, constrained to `[0, 1]` (d3 `paddingInner`).
  - `:padding-out` (default `0.0`): space before the first and after the last band as a proportion of the step (d3 `paddingOuter`), non-negative.
  - `:align-outer` (default `0.5`): how the outer space is distributed: `0.0` all after the last band, `1.0` all before the first band (d3 `align`).
  - `:align` (default `0.5`): position of the point inside every band, from `0.0` (band start) to `1.0` (band end); also `:spread` (positions spread from `0.0` to `1.0` over the bands) or a sequence of positions (cycled).
  - `:round` (default `false`): when truthy, the step is floored and the band start and width are rounded, as d3 `round`.
  - `:unknown` (default `nil`): returned for values outside the domain.

  Forward returns the point of a band; with a second truthy argument it returns the band map:

  - `:value`: the domain value.
  - `:rstart`, `:rend`: the band ends; `:rstart` is the end nearer the range start, so for a reversed range `:rstart` is larger than `:rend`.
  - `:point`: the point at `:align` from `:rstart` to `:rend`.

  The band width and the step (distance between band starts) are stored under `:bandwidth` and `:step`, the band maps under `:bands` in [[data]]. The inverse returns the domain value of the band containing a number (ends included), `:unknown` for padding and values outside the range. `:padding-in` is constrained to `[0, 1]`; `1.0` gives bands of zero width (as the `:point` kind). An empty domain gives no bands.

  Ticks are the domain values; `nice` returns the scale unchanged."
  (:require [fastmath.core :as m]

            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params missing-value? js-round]]
            [wadogo.utils :refer [build-seq ->extent]])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ^:no-doc distinct-values
  "Internal. A sequence domain without repeated values (first occurrences kept, as d3), a count unchanged."
  [domain]
  (if (sequential? domain) (vec (distinct domain)) domain))

(defn- decode-align
  [align ^long bands-no]
  (cond
    (number? align) (repeat bands-no (m/constrain ^double align 0.0 1.0))
    (= :spread align) (m/slice-range bands-no)
    (sequential? align) (take bands-no (cycle align))
    :else (repeat bands-no 0.5)))

(defn- bands-inverse-fn
  "Domain value of the band containing a number, `unknown` otherwise."
  [bands unknown]
  (fn [v]
    (if (missing-value? v)
      unknown
      (let [v (double v)]
        (or (some (fn [{:keys [^double rstart ^double rend value]}]
                    (when (<= (min rstart rend) v (max rstart rend)) value)) bands)
            unknown)))))

(defn ^:no-doc band-layout
  "Internal. Bands of `n` domain values inside the range `r` (`Vec2`, any order) as d3 `scaleBand`, for `paddings` (`Vec3` of inner padding, outer padding and outer align): `Vec3` of `start`, `step` and `bandwidth`, where `start` is the lowest band start; band `i` starts at `start + i * step` (counted from the range end for a reversed range)."
  ^Vec3 [^long n ^Vec2 r ^Vec3 paddings round?]
  (let [lo (min (.x r) (.y r))
        hi (max (.x r) (.y r))
        padding-in (.x paddings)
        step (/ (- hi lo) (max 1.0 (+ (- n padding-in) (* 2.0 (.y paddings)))))
        step (if round? (m/floor step) step)
        start (+ lo (* (- hi lo (* step (- n padding-in))) (.z paddings)))
        bandwidth (* step (- 1.0 padding-in))]
    (if round?
      (Vec3. (js-round start) step (js-round bandwidth))
      (Vec3. start step bandwidth))))

(defn ^:no-doc band-maps
  "Internal. Band maps (`:value`, `:rstart`, `:rend`, `:point`) of the domain `values` for the `layout` from [[band-layout]], the range `r` (`Vec2`) and point positions `aligns`."
  [values ^Vec2 r ^Vec3 layout aligns]
  (let [n (count values)
        reversed? (< (.y r) (.x r))
        start (.x layout)
        step (.y layout)
        bandwidth (.z layout)]
    (map (fn [^long i v ^double align]
           (let [lstart (+ start (* step (if reversed? (- n i 1) i)))
                 lend (+ lstart bandwidth)
                 [rs re] (if reversed? [lend lstart] [lstart lend])]
             {:value v
              :rstart rs
              :rend re
              :point (m/lerp rs re align)})) (range n) values aligns)))

(defmethod scale :bands
  ([_] (scale :bands {}))
  ([s params]
   (let [params (merge-params s params)
         [^double rstart ^double rend] (->extent (:range params))
         [^long bands-no bands] (build-seq (distinct-values (:domain params)))
         {:keys [^double padding-in ^double padding-out ^double align-outer unknown]} params
         r (Vec2. rstart rend)
         ^Vec3 layout (band-layout bands-no r (Vec3. (m/constrain padding-in 0.0 1.0) padding-out (m/constrain align-outer 0.0 1.0))
                             (:round params))
         lst (band-maps bands r layout (decode-align (:align params) bands-no))
         forward (zipmap bands lst)]
     (->ScaleType :bands bands [rstart rend] (:ticks params) (:formatter params)
                  (fn local-forward
                    ([v] (local-forward v false))
                    ([v interval?]
                     (if-let [res (forward v)]
                       (if interval? res (:point res))
                       unknown)))
                  (bands-inverse-fn lst unknown)
                  (assoc (strip-keys params)
                         :bandwidth (.z layout)
                         :step (.y layout)
                         :bands lst)))))

(m/unuse-primitive-operators)
