(ns wadogo.scale.datetime
  "`:datetime` kind: a datetime domain mapped linearly to a continuous numerical range (d3 `scaleUtc`).

  Attributes:

  - `:domain` (default: the year before the moment `wadogo.config` was loaded): two datetimes in any order, a map with `:start` and `:end`, or a longer sequence (reduced to its earliest and latest value). Accepted types: `LocalDateTime`, `LocalDate` (start of the day), `LocalTime` (today), `java.util.Date`, and `Instant`, `ZonedDateTime` and `OffsetDateTime` (converted to the UTC local datetime).
  - `:range` (default `[0.0 1.0]`): as for the `:linear` kind.
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind.

  A degenerate domain (equal ends) maps earlier datetimes to the range start, later ones to the range end and the domain datetime to the middle; it has no default ticks. A degenerate range behaves the same way in the inverse. `LocalTime` values are placed on the current date at each conversion, so a scale built from times shouldn't outlive the day.

  The domain is stored as `LocalDateTime` values. Forward accepts the same types as the domain and is linear in the elapsed time (millisecond precision with the nanosecond fraction); the inverse returns a `LocalDateTime` rounded to the millisecond. The domain length in milliseconds (`BigDecimal`, negative for a reversed domain) is stored under `:millis` in [[data]].

  Default ticks (`:ticks-method :default`, 10 ticks) are spaced by a whole number of the time unit nearest to the tick step, starting at the domain start truncated to that unit, strictly inside the domain (not aligned to calendar units); `:pretty` ticks (5 ticks) follow R's `pretty()` for date-times. Ticks of a reversed domain are reversed. Values outside the domain are extrapolated (unless clamped). `nice` extends the domain to whole time units (`:default`) or to the outer pretty ticks (`:pretty`)."
  (:require [fastmath.core :as m]
            [java-time :as dt]

            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [datetime-diff-millis ->extent make-norm]])
  (:import [java.time Instant ZonedDateTime OffsetDateTime ZoneOffset LocalDateTime]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private epoch (dt/local-date-time 1970 1 1 0 0 0 0))

(defn- ->ldt
  "Datetime value as `LocalDateTime` (UTC for instants and zoned values)."
  [ld]
  (cond
    (instance? LocalDateTime ld) ld
    (dt/local-date? ld) (dt/truncate-to (dt/local-date-time ld) :days)
    (dt/local-time? ld) (dt/local-date-time ld)
    (instance? Instant ld) (LocalDateTime/ofInstant ^Instant ld ZoneOffset/UTC)
    (instance? ZonedDateTime ld) (LocalDateTime/ofInstant (.toInstant ^ZonedDateTime ld) ZoneOffset/UTC)
    (instance? OffsetDateTime ld) (LocalDateTime/ofInstant (.toInstant ^OffsetDateTime ld) ZoneOffset/UTC)
    (instance? java.util.Date ld) (->> (dt/instant ld)
                                       (dt/to-millis-from-epoch)
                                       (dt/millis)
                                       (dt/plus epoch))
    :else ld))

(defn- datetime-forward
  "Function returning the range value of a datetime; for an empty domain (`total` zero) as `wadogo.utils/make-norm`: `rstart` before, `rend` after, the middle at the domain."
  [start ^BigDecimal total ^double rstart ^double rend]
  (if (zero? (.signum total))
    (let [degenerate-norm (make-norm 0.0 0.0 rstart rend)]
      (fn ^double [tm] (degenerate-norm (.signum (datetime-diff-millis start (->ldt tm))))))
    (fn ^double [tm]
      (m/lerp rstart rend (-> (datetime-diff-millis start (->ldt tm))
                              (.divide total java.math.MathContext/DECIMAL128)
                              (.doubleValue))))))

(defn- datetime-inverse
  "Function returning the datetime of a range value; for an empty range (`rstart` equal to `rend`) as `wadogo.utils/make-norm`: the domain start below, the end above, the middle at the range value."
  [start ^BigDecimal total ^double rstart ^double rend]
  (let [range-norm (make-norm rstart rend 0.0 1.0)]
    (fn [^double t]
      (->> (BigDecimal. ^double (range-norm t))
           (.multiply total)
           (m/round)
           (dt/millis)
           (dt/plus start)))))

(defn- ->dt-extent
  "Domain ends as `LocalDateTime`: a map's `:start` and `:end`, a pair as given, or the earliest and latest value."
  [input]
  (let [input (if (map? input) ((juxt :start :end) input) (map ->ldt input))]
    (if (== 2 (count input))
      (mapv ->ldt input)
      [(reduce dt/min input) (reduce dt/max input)])))

(defmethod scale :datetime
  ([_] (scale :datetime {}))
  ([s params]
   (let [params (merge-params s params)
         [start end] (->dt-extent (:domain params))
         [rstart rend] (->extent (:range params))
         total (datetime-diff-millis start end)]
     (continuous-scale :datetime [start end] [rstart rend] params
                       (datetime-forward start total rstart rend)
                       (datetime-inverse start total rstart rend)
                       (assoc (strip-keys params) :millis total)
                       ->ldt))))

(m/unuse-primitive-operators)
