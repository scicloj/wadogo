(ns wadogo.utils
  "Internal helpers shared by the scale kinds, ticks and formatters: powers of ten, normalisation, interval lookup, domain parsing and datetime units.

  All functions are internal (`:no-doc`) and can change without notice."
  (:require [fastmath.core :as m]
            [java-time :as dt]
            [fastmath.stats :as stats])
  (:import [java.time LocalDateTime]
           [fastmath.java Array]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ^:no-doc exp10
  "Internal. `10^k`, exact (closest double) for integer `k` in [-22,22]: both `10^|k|` and the division are exact or correctly rounded."
  ^double [^double k]
  (cond
    (and (>= k 0.0) (<= k 22.0)) (m/pow 10.0 k)
    (and (< k 0.0) (>= k -22.0)) (/ 1.0 (m/pow 10.0 (- k)))
    :else (m/pow 10.0 k)))

(defn ^:no-doc interval-steps-before
  "Internal. Function mapping a number to the index of its interval for non-decreasing `steps` (as d3 `bisectRight`, limited to the last index): `0` below the first step, the number of steps less than or equal to the value otherwise, at most `count(steps) - 1`."
  [steps]
  (let [arr (m/seq->double-array steps)
        n (alength arr)
        last-id (max 0 (dec n))]
    (fn ^long [^double v]
      (loop [lo (long 0) hi n]
        (if (< lo hi)
          (let [mid (bit-shift-right (+ lo hi) 1)]
            (if (< v (Array/aget arr mid))
              (recur lo mid)
              (recur (inc mid) hi)))
          (min lo last-id))))))

(defn ^:no-doc make-norm
  "Internal. Linear map from `[dstart, dend]` to `[rstart, rend]` (extrapolating outside).

  For a degenerate interval (`dstart` equal to `dend`) values below map to `rstart`, values above to `rend` and the value itself to the middle of the range."
  [^double dstart ^double dend ^double rstart ^double rend]
  (if (== dstart dend)
    (let [half (m/mlerp rstart rend 0.5)]
      (fn [^double v]
        (cond
          (< v dstart) rstart
          (> v dstart) rend
          :else half)))
    (m/make-norm dstart dend rstart rend)))

(defn ^:no-doc build-seq
  "Internal. `[n values]` from a sequence (its count and the sequence) or a count `n` (and `0..n-1`); `nil` otherwise."
  [count-or-seq]
  (cond
    (sequential? count-or-seq) [(count count-or-seq) count-or-seq]
    (number? count-or-seq) [count-or-seq (range count-or-seq)]
    :else nil))

(defn ^:no-doc values->reversed-map
  "Internal. Map from the value under the key `k` to the first map in the sequence `values` holding that value."
  [values k]
  (reduce (fn [curr m]
            (if (curr (m k)) curr
                (assoc curr (m k) m))) {} values))

;; domain/range parser

(defn ^:no-doc ->extent
  "Internal. Domain or range ends: `[start end]` of a map with `:start` and `:end`, a two-element sequence as given (order kept), or `[min max]` of a longer sequence."
  [input]
  (cond
    (map? input) ((juxt :start :end) input)
    (not= (count input) 2) (take 2 (stats/extent input))
    :else input))

;; datetime

(defn ^:no-doc datetime-diff-millis
  "Internal. Time from `start` to `end` in milliseconds (with the nanosecond fraction) as a `BigDecimal`, negative when `end` is before `start`."
  ^BigDecimal [start end]
  (let [dur (dt/duration start end)
        seconds (BigDecimal. ^long (dt/value (dt/property dur :seconds)))
        nanos (.divide (BigDecimal. ^long (dt/value (dt/property dur :nanos))) 1000000.0M)]
    (.add nanos (.multiply seconds 1000.0M))))

(def ^:private ^:const duration-second 1000)
(def ^:private ^:const duration-minute (* duration-second 60))
(def ^:private ^:const duration-hour (* duration-minute 60))
(def ^:private ^:const duration-day (* duration-hour 24))
(def ^:private ^:const duration-month (* duration-day 30))
(def ^:private ^:const duration-year (* duration-day 365))

(def ^{:no-doc true
       :doc "Internal. Datetime units (`:years` to `:millis`) with their property name, approximate duration in milliseconds, step function (count to a period) and truncation function."}
  dt-data
  {:years {:property :year
           :duration duration-year
           :stepfn dt/years
           :truncate #(dt/truncate-to (dt/adjust % :first-day-of-year) :days)}
   :months {:property :month-of-year
            :duration duration-month
            :stepfn dt/months
            :truncate #(dt/truncate-to (dt/adjust % :first-day-of-month) :days)}
   :days {:property :day-of-month
          :duration duration-day
          :stepfn dt/days
          :truncate #(dt/truncate-to % :days)}
   :hours {:property :hour-of-day
           :duration duration-hour
           :stepfn dt/hours
           :truncate #(-> ^LocalDateTime %
                          (.withNano 0)
                          (.withMinute 0)
                          (.withSecond 0))}
   :minutes {:property :minute-of-hour
             :duration duration-minute
             :stepfn dt/minutes
             :truncate #(-> ^LocalDateTime %
                            (.withNano 0)
                            (.withSecond 0))}
   :seconds {:property :second-of-minute
             :duration duration-second
             :stepfn dt/seconds
             :truncate #(-> ^LocalDateTime %
                            (.withNano 0))}
   :millis {:property :millis-of-second
            :duration 1
            :stepfn dt/millis
            :truncate identity}})

(defn ^:no-doc step->duration
  "Internal. Largest datetime unit (see [[dt-data]]) shorter than `step` milliseconds, `:millis` for one second or less."
  [^double step]
  (cond
    (> step duration-year) :years
    (> step duration-month) :months
    (> step duration-day) :days
    (> step duration-hour) :hours
    (> step duration-minute) :minutes
    (> step duration-second) :seconds
    :else :millis))

;;

(defn ^:no-doc build-forward
  "Internal. Function of a double: `forward` followed by `norm` (used by `defcustom`)."
  [forward norm]
  (fn ^double [^double x]
    (norm (forward x))))

(defn ^:no-doc build-inverse
  "Internal. Function of a double: `norm` followed by `inverse` (used by `defcustom`)."
  [inverse norm]
  (fn ^double [^double x]
    (inverse (norm x))))

(m/unuse-primitive-operators)
