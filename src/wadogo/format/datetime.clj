(ns wadogo.format.datetime
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
  [xs]
  (->> xs
       (partition 2 1)
       (map (fn [[s e]] (datetime-diff-millis s e)))
       (apply min)))

(defn time-format
  [xs]
  (let [step (find-minimum-step xs)
        fmt (format-dt-str xs step)]
    (partial dt/format fmt)))
