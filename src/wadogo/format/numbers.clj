(ns wadogo.format.numbers
  "Formatters of numbers, used by `wadogo.scale/format` for numerical ticks.

  [[formatter]] writes doubles with a fixed or the shortest exact number of decimal places, switching to scientific notation for large and small magnitudes; [[int-formatter]] writes integers (decimal or hexadecimal, optionally zero padded). Both take the map given as the `:formatter-params` attribute of a scale."
  (:require [fastmath.core :as m]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; maximum double power for precise calculations
(def ^:private ^:const kp-max 22)

;; powers for scientific notation
(def ^:private tbl [1e-1,
                    1e00, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06, 1e07, 1e08, 1e09,
                    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
                    1e20, 1e21, 1e22])

(defn- left
  "What is the power of number"
  ^long [^double x]
  (-> x m/log10 m/floor unchecked-long inc))

(defn- find-nsig
  "Shift decimal places until non-zero value is found"
  ^long [^double alpha ^long digits]
  (loop [a alpha
         d digits]
    (let [a- (/ a 10.0)]
      (if (= a- (m/floor a-))
        (recur a- (dec d))
        (max 1 d)))))

(defn- right
  "Calculate maximum digits on the right side of the dot."
  ^long [^double x ^long digits]
  (let [alpha (double (m/round (* x ^double (tbl (inc digits)))))]
    (if (zero? alpha)
      1
      (find-nsig alpha digits))))

(defn- precision
  [^double x ^long digits ^long threshold]
  (if (zero? x)
    [(not (pos? threshold)) 0 1 1] ;; zero is reprezented as 0.0
    (let [digits (max 1 (min 10 digits)) ;; constrain digits to 1-10 range
          r (m/abs x)
          lft (left r) ;; digits on the left side of dot
          alft (m/abs lft)
          e? (>= alft threshold)
          r-prec (cond
                   (< alft threshold) r ;; normal number
                   (< alft kp-max) (if (neg? lft) ;; scientific number (using table to shift values)
                                     (* r ^double (tbl (inc (- lft))))
                                     (/ r ^double (tbl (inc lft))))
                   :else (/ r (m/pow 10.0 (dec lft)))) ;; very big or very small case
          rght (right r-prec digits) ;; desired precision on the right side
          ]
      [e? rght])))

(defn formatter
  "Creates a function formatting a number as a string.

  Parameters (a map, optional):

  - `:digits` (default `-6`): a positive value is the fixed number of decimal places; a negative value `-d` uses the fewest decimal places (at least one, at most `d`) that show the number rounded to `d` places.
  - `:threshold` (default `8`): scientific notation (as `1.0E+10`) is used when the number has at least this many digits before the decimal point, or this many zeros after it.
  - `:na` (default `NA`): text of `nil`.
  - `:nan` (default `NaN`), `:inf` (default `∞`), `:-inf` (default `-∞`): texts of the invalid doubles.

  Integers are formatted as doubles (`3` gives `3.0`). Every number is formatted on its own, so the number of decimal places can differ between values. The number of decimal places is limited to 10.

  See also [[int-formatter]], [[wadogo.format.ranges/range-formatter]]."
  ([] (formatter {}))
  ([{:keys [^long digits threshold na nan inf -inf]
     :or {digits -6 threshold 8 na "NA" nan "NaN" inf "∞" -inf "-∞"}}]
   (fn [x]
     (if x
       (let [cut? (neg? digits)
             digits (if cut? (- digits) digits)
             [e? right] (precision x digits threshold)
             digits (max 1 (long (if cut? right digits)))
             x (if (or (double? x)
                       (float? x)) x (double x))]
         (cond
           (m/nan? (double x)) nan
           (m/pos-inf? (double x)) inf
           (m/neg-inf? (double x)) -inf
           
           (zero? (double x)) (let [zeros (str/join (repeat digits "0"))]
                                (if e? (str "0." zeros "E+00") (str "0." zeros)))
           :else (format (str "%." digits (if e? "E" "f")) x)))
       na))))

(defn int-formatter
  "Creates a function formatting an integer as a string.

  Parameters (a map, optional):

  - `:digits` (default `0`): when positive, the minimal width, padded with leading zeros.
  - `:hex?` (default `false`): when truthy, hexadecimal with the `0x` prefix.
  - `:na` (default `NA`): text of `nil`.

  The value is converted with `int`, so fractions are truncated and values outside the `int` range throw.

  See also [[formatter]]."
  ([] (int-formatter {}))
  ([{:keys [^long digits na hex?]
     :or {digits 0 na "NA" hex? false}}]
   (let [pad (when (pos? digits) (str "0" digits))
         suffix (if hex? "x" "d")
         prefix (when hex? "0x")
         f (str prefix "%" pad suffix)]
     (fn [x] (if x (format f (int x)) na)))))

(m/unuse-primitive-operators)
