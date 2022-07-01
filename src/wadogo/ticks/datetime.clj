(ns wadogo.ticks.datetime
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
  "Date time ticks."
  [start end diff ^long ticks]
  (let [step (diff->step diff ticks)]
    (calc-dt-ticks start end step)))
