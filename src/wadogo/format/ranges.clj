(ns wadogo.format.ranges
  "Formatter of intervals given as pairs `[left right]`, used for scales with interval values (as the `:threshold` kind with `:range :intervals`)."
  (:require [wadogo.format.numbers :refer [formatter]]
            [fastmath.core :as m]))

(defn range-formatter
  "Creates a function formatting an interval `[left right]` as a string.

  Parameters (a map, optional; the same map as the `:formatter-params` attribute of a scale):

  - `:endpoints` (default `:open`): with `:open` an interval with an infinite end is written as `< right` or `≥ left`; any other value writes it in interval notation, as `(-∞, right)` and `[left, ∞)`.
  - the number formatting keys of [[wadogo.format.numbers/formatter]] (as `:digits`), used for both ends.

  Finite intervals are written as `[left, right)`. An infinite or `NaN` end counts as unbounded."
  ([] (range-formatter {}))
  ([formatter-params]
   (let [formatter-params (merge {:endpoints :open} formatter-params)
         method (:endpoints formatter-params)
         fmtr (formatter formatter-params)]
     (fn [[left right]]
       (cond (and (m/invalid-double? left) (= method :open)) (str "< " (fmtr right))
             (and (m/invalid-double? right) (= method :open)) (str "≥ " (fmtr left))
             :else (str (if (m/invalid-double? left) "(" "[") (fmtr left) ", " (fmtr right) ")"))))))

(comment
  (def f1 (range-formatter))
  (def f2 (range-formatter {:endpoints nil}))

  (f1 [##-Inf 3])
  ;; => "< 3.0"
  (f1 [3 ##Inf])
  ;; => "≥ 3.0"
  (f1 [10 30])
  ;; => "[10.0, 30.0)"

  (f2 [##-Inf 3])
  ;; => "(-∞, 3.0)"
  (f2 [3 ##Inf])
  ;; => "[3.0, ∞)"
  (f2 [10 30])
  ;; => "[10.0, 30.0)"
  )
