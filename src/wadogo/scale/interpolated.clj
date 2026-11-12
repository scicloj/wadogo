(ns wadogo.scale.interpolated
  "`:interpolated` kind: piecewise mapping through `(domain, range)` pairs with an interpolation method.

  Attributes:

  - `:domain` (default `[0.0 0.5 1.0]`), `:range` (default `[0.0 0.5 1.0]`): sequences of numbers of equal length, paired by position; the pairs are sorted by the domain value. Extra values of the longer sequence are dropped (also from the stored domain and range). Repeated domain values are rejected by most `fastmath` interpolators with `ex-info` when the scale is created.
  - `:interpolator` (default `:linear`): a `fastmath.interpolation/interpolation` method keyword (as `:linear`, `:pchip`, `:monotone`, `:step`) or a function of `xs`, `ys` and the parameters returning a function of one number.
  - `:interpolator-params` (default `nil`): a sequence of extra arguments of the interpolator, as `[{:bandwidth 0.7}]` for `:loess`.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind; the clamping interval is spanned by the smallest and the largest domain value.

  A scale should be monotone: use monotone data with a monotonicity preserving method (`:linear`, `:pchip`, `:monotone`, step methods). `:cubic`, `:loess`, `:shepard` and similar can overshoot between the points.

  The inverse solves `forward(x) = y` by bisection, so it's the inverse of the forward curve for any monotone method. The direction (increasing or decreasing) is given by the forward values at the smallest and the largest domain value. The inverse returns the smallest domain value when `forward` gives `y` there, otherwise the smallest `x` with `forward(x)` at or beyond `y` (in the direction of the curve), so a plateau of a step method gives its start, and a value inside a jump gives the jump position.

  Values beyond the forward values at the domain ends are searched outside the domain with growing steps, as long as the extrapolated curve keeps moving toward the value; `NaN` when it turns back (smooth methods extrapolate with turning polynomials), stays constant (step methods), gives `NaN`, or doesn't reach the value. With `:clamp` such values give the nearer domain end instead.

  Ticks and `nice` use the first and last domain value; `nice` replaces only these two."
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]

            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private ^:const max-extensions 64)
(def ^:private ^:const first-step-fraction (/ 1024.0)) ;; first step outside the domain, as a fraction of its width
(def ^:private ^:const max-bisections 2200) ;; enough to reach adjacent doubles over any finite interval

(defn- interpolate
  "Interpolation function through `pairs` sorted by their first element."
  [interpolator params pairs]
  (let [pairs (sort-by first pairs)
        xs (map first pairs)
        ys (map second pairs)]
    (if (fn? interpolator)
      (apply interpolator xs ys params)
      (apply i/interpolation interpolator xs ys params))))

(defn- bisect
  "Smallest `x` in the `bracket` `[lo, hi]` with `(reached? x)`, up to adjacent doubles.

  Precondition: `(reached? hi)`, and not `(reached? lo)` unless `lo` equals `hi`. Returns an `x` with `(reached? x)`."
  ^double [reached? ^Vec2 bracket]
  (loop [lo (.x bracket) hi (.y bracket) i (long 0)]
    (let [mid (+ lo (* 0.5 (- hi lo)))]
      (if (or (<= mid lo) (>= mid hi) (>= i max-bisections))
        hi
        (if (reached? mid)
          (recur lo mid (inc i))
          (recur mid hi (inc i)))))))

(defn- extend-bracket
  "Bracket of `y` outside the domain, searched from a domain end with growing steps, as long as `f` keeps moving toward `y`.

  `search` is a `Vec3` of the domain end, the first step and the direction (`-1.0` left, `1.0` right); steps double.

  Returns a `Vec2` `[lo hi]` for [[bisect]] (`(reached? hi)`, and not `(reached? lo)` unless `lo` equals `hi`), or `nil` when `f` turns back, stays constant, gives `NaN` or doesn't reach `y` in [[max-extensions]] steps."
  [f reached? ^double y ^Vec3 search]
  (let [side (.z search)
        end (.x search)
        toward (m/sgn (- y (double (f end))))]
    (loop [prev end fprev (double (f end)) step (.y search) k (long 0)]
      (let [x (+ prev (* side step))
            fx (double (f x))]
        (cond
          (or (m/nan? fx) (not (pos? (* toward (- fx fprev)))) (>= k max-extensions)) nil
          (neg? (* toward (- fx y))) (recur x fx (* 2.0 step) (inc k))
          (pos? side) (Vec2. prev x)
          (reached? x) (Vec2. x x) ;; f(x) equals y
          :else (Vec2. x prev))))))

(defn- monotone-inverse
  "Inverse of the monotone function `f` on `[xmin, xmax]` (`xmin <= xmax`) by bisection, increasing when `(f xmax)` isn't below `(f xmin)`.

  For `y` beyond the values at the ends, the search continues outside `[xmin, xmax]` (see [[extend-bracket]]), or, when `clamp?` is truthy, returns the domain end nearer to `y`. Returns `NaN` when `y` isn't reached."
  [f ^double xmin ^double xmax clamp?]
  (let [f (fn ^double [^double x] (double (f x)))
        fmin (double (f xmin))
        fmax (double (f xmax))
        increasing? (>= fmax fmin)
        first-step (* first-step-fraction (- xmax xmin))]
    (fn [y]
      (let [y (double y)
            ;; reached? is monotone in x: false then true
            reached? (if increasing?
                       (fn [^double x] (>= ^double (f x) y))
                       (fn [^double x] (<= ^double (f x) y)))
            beyond-xmin? (and (not= y fmin) (reached? xmin))
            beyond-xmax? (not (reached? xmax))]
        (cond
          (m/nan? y) ##NaN
          (== y fmin) xmin
          (not (or beyond-xmin? beyond-xmax?)) (bisect reached? (Vec2. xmin xmax))
          clamp? (if beyond-xmin? xmin xmax)
          (not (pos? first-step)) ##NaN
          :else (if-let [bracket (if beyond-xmin?
                                   (extend-bracket f reached? y (Vec3. xmin first-step -1.0))
                                   (extend-bracket f reached? y (Vec3. xmax first-step 1.0)))]
                  (bisect reached? bracket)
                  ##NaN))))))

(defmethod scale :interpolated
  ([_ ] (scale :interpolated {}))
  ([s params]
   (let [params (merge-params s params)
         pairs (map vector (:domain params) (:range params))
         xs (mapv first pairs)
         forward (interpolate (:interpolator params :linear) (:interpolator-params params) pairs)]
     (continuous-scale :interpolated xs (mapv second pairs) params
                       forward
                       (monotone-inverse forward (reduce min xs) (reduce max xs) (:clamp params))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
