(ns wadogo.utils
  (:require [fastmath.interpolation :as i]
            [fastmath.core :as m]
            [java-time :as dt]
            [fastmath.stats :as stats])
  (:import [java.time LocalDateTime]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ensure-seq-content
  "If `a` is empty and we have something in `b` - create seq of consecutive numbers."
  [a b]
  (vec (if (and (not a) b)
         (clojure.core/range (count b))
         a)))

(defn interval-steps-before
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp unchecked-int (i/step-before (map m/prev-double steps)
                                     (clojure.core/range (count steps)))))

(defn make-norm
  "If domain or range has degenerated interval, treat it special"
  [^double dstart ^double dend ^double rstart ^double rend]
  (if (== dstart dend)
    (let [half (m/mlerp rstart rend 0.5)]
      (fn [^double v]
        (cond
          (< v dstart) rstart
          (> v dstart) rend
          :else half)))
    (m/make-norm dstart dend rstart rend)))

(defn build-seq
  [count-or-seq]
  (if (sequential? count-or-seq)
    [(count count-or-seq) count-or-seq]
    [count-or-seq (range count-or-seq)]))

(defn values->reversed-map
  [values k]
  (reduce (fn [curr m]
            (if (curr (m k)) curr
                (assoc curr (m k) m))) {} values))

;; domain/range parser

(defn ->extent
  [input]
  (cond
    (map? input) ((juxt :start :end) input)
    (not= (count input) 2) (take 2 (stats/extent input))
    :else input))

;; datetime

(defn datetime-diff-millis
  "Calculate time duration in milliseconds.nanoseconds."
  ^BigDecimal [start end]
  (let [dur (dt/duration start end)
        seconds (BigDecimal. ^long (dt/value (dt/property dur :seconds)))
        nanos (.divide (BigDecimal. ^long (dt/value (dt/property dur :nanos))) 1000000.0M)]
    (.add nanos (.multiply seconds 1000.0M))))

(def ^:private ^:const ^long duration-second 1000)
(def ^:private ^:const ^long duration-minute (* duration-second 60))
(def ^:private ^:const ^long duration-hour (* duration-minute 60))
(def ^:private ^:const ^long duration-day (* duration-hour 24))
(def ^:private ^:const ^long duration-month (* duration-day 30))
(def ^:private ^:const ^long duration-year (* duration-day 365))

;; date time data set
(def dt-data
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

(defn step->duration
  [^double step]
  (cond
    (> step duration-year) :years
    (> step duration-month) :months
    (> step duration-day) :days
    (> step duration-hour) :hours
    (> step duration-minute) :minutes
    (> step duration-second) :seconds
    :else :millis))
