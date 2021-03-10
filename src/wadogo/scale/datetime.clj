(ns wadogo.scale.datetime
  (:require [fastmath.core :as m]
            [java-time :as dt]

            [wadogo.common :refer [scale ->ScaleType]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private epoch (dt/local-date-time 1970 1 1 0 0 0 0))

(defn- ld->ldt
  "Convert local-date to local-date-time"
  [ld]
  (cond
    (dt/local-date? ld) (dt/truncate-to (dt/local-date-time ld) :days)
    (dt/local-time? ld) (dt/local-date-time ld)
    (instance? java.util.Date ld) (->> (dt/instant ld)
                                       (dt/to-millis-from-epoch)
                                       (dt/millis)
                                       (dt/plus epoch))
    :else ld))

(defn- datetime-diff-millis
  "Calculate time duration in milliseconds.nanoseconds."
  ^BigDecimal [start end]
  (let [dur (dt/duration start end)
        seconds (BigDecimal. ^long (dt/value (dt/property dur :seconds)))
        nanos (.divide (BigDecimal. ^long (dt/value (dt/property dur :nanos))) 1000000.0M)]
    (.add nanos (.multiply seconds 1000.0M))))

(defn- datetime-forward
  "Create function which returns offset from starting date for given temporal value."
  [start ^BigDecimal total]
  (fn ^double [tm]
    (-> (datetime-diff-millis start (ld->ldt tm))
        (.divide total java.math.MathContext/DECIMAL128)
        (.doubleValue))))

(defn- datetime-inverse
  "Create function which returns date-time for given offset from start."
  [start ^BigDecimal total]
  (fn [^double t]
    (->> (BigDecimal. t)
         (.multiply total)
         (m/round)
         (dt/millis)
         (dt/plus start))))

(def ^:private datetime-params
  {:domain [(dt/minus (dt/local-date-time) (dt/years 1)) (dt/local-date-time)]
   :range [0.0 1.0]})

(defmethod scale :datetime
  ([_] (scale :datetime {}))
  ([_ params]
   (let [params (merge datetime-params params)
         [dstart dend] (:domain params)
         start (ld->ldt dstart)
         end (ld->ldt dend)
         total (datetime-diff-millis start end)]
     (->ScaleType :datetime [start end] (:range params)
                  (datetime-forward start total)
                  (datetime-inverse start total)
                  {:millis total}))))
