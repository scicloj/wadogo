(ns wadogo.scale.datetime
  (:require [fastmath.core :as m]
            [java-time :as dt]

            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [datetime-diff-millis ->extent]]))

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

(defn- datetime-forward
  "Create function which returns offset from starting date for given temporal value."
  [start ^BigDecimal total ^double rstart ^double rend]
  (fn ^double [tm]
    (m/lerp rstart rend (-> (datetime-diff-millis start (ld->ldt tm))
                            (.divide total java.math.MathContext/DECIMAL128)
                            (.doubleValue)))))

(defn- datetime-inverse
  "Create function which returns date-time for given offset from start."
  [start ^BigDecimal total ^double rstart ^double rend]
  (fn [^double t]
    (let [tt (m/norm t rstart rend)]
      (->> (BigDecimal. tt)
           (.multiply total)
           (m/round)
           (dt/millis)
           (dt/plus start)))))

(defn- ->dt-extent
  [input]
  (if (map? input)
    ((juxt :start :end) input)
    [(reduce dt/min input) (reduce dt/max input)]))

(defmethod scale :datetime
  ([_] (scale :datetime {}))
  ([s params]
   (let [params (merge-params s params)
         [dstart dend] (->dt-extent (:domain params))
         [rstart rend] (->extent (:range params))
         start (ld->ldt dstart)
         end (ld->ldt dend)
         total (datetime-diff-millis start end)]
     (->ScaleType :datetime [start end] [rstart rend] (:ticks params) (:fmt params)
                  (datetime-forward start total rstart rend)
                  (datetime-inverse start total rstart rend)
                  (assoc (strip-keys params) :millis total)))))
