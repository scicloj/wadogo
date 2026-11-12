(ns wadogo.ticks.datetime
  "Default ticks of the `:datetime` kind: datetimes spaced by a whole number of the time unit nearest to the tick step.

  Ticks start at the domain start truncated to the unit and aren't aligned to calendar units (as weeks or quarters). R `pretty()` date-times are in `wadogo.ticks.pretty-datetime`."
  (:require [fastmath.core :as m]
            [java-time :as dt]

            [wadogo.utils :refer [step->duration dt-data]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- calc-dt-ticks
  "Calculate ticks"
  [start end ^double step]
  (let [{:keys [^double duration stepfn truncate]} (-> step
                                                       step->duration
                                                       dt-data)
        step (->> (/ step duration) m/floor stepfn)]
    (loop [s (dt/plus (truncate start) (stepfn 1))
           v []]
      (if (dt/before? s end)
        (recur (dt/plus s step) (if (dt/after? s start) (conj v s) v))
        v))))

(defn- diff->step
  ^double [^BigDecimal diff ^long ticks]
  (.doubleValue (.divide diff (BigDecimal. ticks) java.math.MathContext/DECIMAL128)))

(defn datetime-ticks
  "Returns datetimes strictly between `start` and `end` spaced by a whole number of time units.

  Parameters:

  - `start`, `end`: `LocalDateTime` domain ends, `start` before `end`.
  - `diff`: the time from `start` to `end` in milliseconds, a `BigDecimal` (see `wadogo.utils/datetime-diff-millis`).
  - `ticks`: the approximate number of ticks, positive.

  The unit is the largest of years, months, days, hours, minutes, seconds and milliseconds shorter than `diff/ticks` (with a year of 365 and a month of 30 days); the step is that many whole units (at least one). Candidates start one unit after the domain start truncated to the unit and follow by the step; those after `start` and before `end` are kept.

  Returns a vector of `LocalDateTime`, empty when `end` isn't after `start`."
  [start end diff ^long ticks]
  (let [step (diff->step diff ticks)]
    (calc-dt-ticks start end step)))

(m/unuse-primitive-operators)
