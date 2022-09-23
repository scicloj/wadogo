(ns wadogo.format.ranges
  (:require [wadogo.format.numbers :refer [formatter]]
            [fastmath.core :as m]))

(defn range-formatter
  ([] (range-formatter {:endpoints :open}))
  ([formatter-params]
   (let [method (:endpoints formatter-params)
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
