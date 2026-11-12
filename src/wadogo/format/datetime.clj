(ns wadogo.format.datetime
  "Formatter of datetime ticks, used by `wadogo.scale/format` for the `:datetime` kind.

  [[time-format]] chooses a `java.time` pattern from the time unit of the smallest step between the values, omitting the parts the values share (as the year when all are in one year)."
  (:require [java-time :as dt]

            [wadogo.utils :refer [dt-data step->duration datetime-diff-millis]]))

(set! *unchecked-math* :warn-on-boxed)

(defn- same-properties?
  "Compare given properties"
  [start end k]
  (let [prop (:property (dt-data k))]
    (= (dt/value (dt/property start prop))
       (dt/value (dt/property end prop)))))

(defn- format-dt-str
  "Infer format"
  [steps step]
  (let [s (first steps)
        e (last steps)
        same? (partial same-properties? s e)]
    (condp = (step->duration step)
      :years "y"
      :months (if-not (same? :years) "y-MM" "MMM")
      :days (if-not (same? :years)
              "y-MM-dd"
              (if-not (same? :months) "MMM-dd" "dd"))
      :hours (if-not (and (same? :years)
                          (same? :months)
                          (same? :days))
               "E HH:mm" "HH:mm")
      :minutes "HH:mm"
      :seconds (if-not (same? :minutes) "HH:mm:ss" "ss")
      :millis (if-not (same? :seconds) "ss.S" "S"))))

(defn- find-minimum-step
  "Smallest absolute time between consecutive `xs` in milliseconds."
  [xs]
  (->> xs
       (partition 2 1)
       (map (fn [[s e]] (.abs ^java.math.BigDecimal (datetime-diff-millis s e))))
       (reduce min)))

(defn time-format
  "Creates a function formatting a datetime with a pattern chosen for the values `xs`.

  Parameters:

  - `xs`: the datetime values to be formatted (usually ticks), `LocalDateTime` values in any order.
  - `domain` (optional): two datetimes used instead of `xs` when there are fewer than two values.

  The pattern follows the time unit of the smallest step between consecutive values: `y` for years, `y-MM` or `MMM` for months, `y-MM-dd`, `MMM-dd` or `dd` for days, `E HH:mm` or `HH:mm` for hours, `HH:mm` for minutes, `HH:mm:ss` or `ss` for seconds, `ss.S` or `S` for milliseconds; the shorter variants are used when the first and last value share the larger units. With fewer than two values and no domain, the full pattern `y-MM-dd HH:mm:ss` is used."
  ([xs] (time-format xs nil))
  ([xs domain]
   (let [xs (if (and (< (count xs) 2) (= 2 (count domain))) domain xs)
         fmt (if (< (count xs) 2)
               "y-MM-dd HH:mm:ss"
               (format-dt-str xs (.doubleValue ^java.math.BigDecimal (find-minimum-step xs))))]
     (partial dt/format fmt))))
