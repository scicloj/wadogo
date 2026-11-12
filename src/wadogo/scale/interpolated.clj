(ns wadogo.scale.interpolated
  "`:interpolated` kind: piecewise mapping through `(domain, range)` pairs with an interpolation method.

  Attributes:

  - `:domain` (default `[0.0 0.5 1.0]`), `:range` (default `[0.0 0.5 1.0]`): sequences of numbers of equal length, paired by position; the pairs are sorted by the domain value.
  - `:interpolator` (default `:linear`): a `fastmath.interpolation/interpolation` method keyword (as `:linear`, `:cubic`, `:loess`, `:step`) or a function of `xs`, `ys` and the parameters returning a function of one number.
  - `:interpolator-params` (default `nil`): a sequence of extra arguments of the interpolator, as `[{:bandwidth 0.7}]` for `:loess`.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind; the clamping interval is spanned by the smallest and the largest domain value.

  The inverse interpolates the pairs swapped (range to domain, sorted by the range value) with the same method, so it's exact only for monotone data.

  Ticks and `nice` use the first and last domain value; `nice` replaces only these two."
  (:require [fastmath.interpolation :as i]

            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]))

(defn- interpolate
  "Interpolation function through `pairs` sorted by their first element."
  [interpolator params pairs]
  (let [pairs (sort-by first pairs)
        xs (map first pairs)
        ys (map second pairs)]
    (if (fn? interpolator)
      (apply interpolator xs ys params)
      (apply i/interpolation interpolator xs ys params))))

(defmethod scale :interpolated
  ([_ ] (scale :interpolated {}))
  ([s params]
   (let [params (merge-params s params)
         pairs (map vector (:domain params) (:range params))
         interpolator (:interpolator params :linear)
         interpolator-params (:interpolator-params params)]
     (continuous-scale :interpolated (:domain params) (:range params) params
                       (interpolate interpolator interpolator-params pairs)
                       (interpolate interpolator interpolator-params (map (comp vec reverse) pairs))
                       (strip-keys params)))))
