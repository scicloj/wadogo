(ns wadogo.ticks.pretty-datetime
  "Date-time ticks based on R's `prettyDate()` (`src/library/grDevices/R/prettyDate.R`).

  `LocalDateTime` values are treated as UTC `POSIXct`. The date-only branch of `prettyDate()` is not ported.
  Only ticks inside the domain are returned by `pretty-datetime-ticks`; `pretty-datetime-nice-bounds` returns the outer ticks (used by `nice`)."
  (:require [fastmath.core :as m]
            [wadogo.ticks.pretty :refer [validate-pretty-params]])
  (:import [java.time LocalDateTime ZoneOffset DayOfWeek Duration]
           [java.time.temporal ChronoUnit TemporalAdjusters]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const MIN 60.0)
(def ^:private ^:const HOUR (* MIN 60.0))
(def ^:private ^:const DAY (* HOUR 24.0))
(def ^:private ^:const YEAR (* DAY 365.25))
(def ^:private ^:const MONTH (/ YEAR 12.0))

;; pretty steps: approximate length in seconds, stepping unit and amount, truncation unit of the first tick
(def ^:private steps
  [{:secs 1.0 :unit :seconds :amount 1 :start :mins}
   {:secs 2.0 :unit :seconds :amount 2 :start :mins}
   {:secs 5.0 :unit :seconds :amount 5 :start :mins}
   {:secs 10.0 :unit :seconds :amount 10 :start :mins}
   {:secs 15.0 :unit :seconds :amount 15 :start :mins}
   {:secs 30.0 :unit :seconds :amount 30 :start :mins}
   {:secs MIN :unit :minutes :amount 1 :start :mins}
   {:secs (* 2.0 MIN) :unit :minutes :amount 2 :start :hours}
   {:secs (* 5.0 MIN) :unit :minutes :amount 5 :start :hours}
   {:secs (* 10.0 MIN) :unit :minutes :amount 10 :start :hours}
   {:secs (* 15.0 MIN) :unit :minutes :amount 15 :start :hours}
   {:secs (* 30.0 MIN) :unit :minutes :amount 30 :start :hours}
   {:secs HOUR :unit :hours :amount 1 :start :hours}
   {:secs (* 3.0 HOUR) :unit :hours :amount 3 :start :days}
   {:secs (* 6.0 HOUR) :unit :hours :amount 6 :start :days}
   {:secs (* 12.0 HOUR) :unit :hours :amount 12 :start :days}
   {:secs DAY :unit :days :amount 1 :start :days}
   {:secs (* 2.0 DAY) :unit :days :amount 2 :start :days}
   {:secs (* 7.0 DAY) :unit :days :amount 7 :start :weeks}
   {:secs (/ MONTH 2.0) :unit :halfmonth :amount 1 :start :months}
   {:secs MONTH :unit :months :amount 1 :start :months}
   {:secs (* 3.0 MONTH) :unit :months :amount 3 :start :years}
   {:secs (* 6.0 MONTH) :unit :months :amount 6 :start :years}
   {:secs YEAR :unit :years :amount 1 :start :years}
   {:secs (* 2.0 YEAR) :unit :years :amount 2 :start :decades}
   {:secs (* 5.0 YEAR) :unit :years :amount 5 :start :decades}
   {:secs (* 10.0 YEAR) :unit :years :amount 10 :start :decades}
   {:secs (* 20.0 YEAR) :unit :years :amount 20 :start :centuries}
   {:secs (* 50.0 YEAR) :unit :years :amount 50 :start :centuries}
   {:secs (* 100.0 YEAR) :unit :years :amount 100 :start :centuries}
   {:secs (* 200.0 YEAR) :unit :years :amount 200 :start :centuries}
   {:secs (* 500.0 YEAR) :unit :years :amount 500 :start :centuries}
   {:secs (* 1000.0 YEAR) :unit :years :amount 1000 :start :centuries}])

(defn- seconds-between
  "Duration from `a` to `b` in seconds."
  ^double [^LocalDateTime a ^LocalDateTime b]
  (let [d (Duration/between a b)]
    (+ (.getSeconds d) (* 1.0e-9 (.getNano d)))))

(defn- epoch-seconds ^double [^LocalDateTime t]
  (+ (.toEpochSecond t ZoneOffset/UTC) (* 1.0e-9 (.getNano t))))

(defn- from-epoch-seconds ^LocalDateTime [^long s]
  (LocalDateTime/ofEpochSecond s 0 ZoneOffset/UTC))

(defn- truncate
  "R's `trunc_POSIXt` (weeks start on Monday)."
  ^LocalDateTime [^LocalDateTime t unit]
  (let [day (.truncatedTo t ChronoUnit/DAYS)]
    (case unit
      :mins (.truncatedTo t ChronoUnit/MINUTES)
      :hours (.truncatedTo t ChronoUnit/HOURS)
      :days day
      :weeks (.with day (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY))
      :months (.withDayOfMonth day 1)
      :years (.withDayOfYear day 1)
      :decades (.withYear (.withDayOfYear day 1) (* 10 (Math/floorDiv (long (.getYear day)) 10)))
      :centuries (.withYear (.withDayOfYear day 1) (* 100 (Math/floorDiv (long (.getYear day)) 100))))))

(defn- add-steps
  "`t` moved by `k` steps (not for half months)."
  ^LocalDateTime [^LocalDateTime t {:keys [unit ^long amount]} ^long k]
  (let [a (* k amount)]
    (case unit
      :seconds (.plusSeconds t a)
      :minutes (.plusMinutes t a)
      :hours (.plusHours t a)
      :days (.plusDays t a)
      :months (.plusMonths t a)
      :years (.plusYears t a))))

(defn- half-month-shift
  "The half month point following the month-sequence point `t` (`seqDtime`)."
  ^LocalDateTime [^LocalDateTime t]
  (let [md (.getDayOfMonth t)]
    (cond
      (== md 1) (.withDayOfMonth t 15)
      (>= md 15) (.withDayOfMonth (.plusMonths t 1) 1)
      :else (.withDayOfMonth t (+ md 14)))))

(defn- step-seq
  "`seqDtime(beg, end, by = step)`: points from `beg` not later than `end`.
  Half months interleave month points with their shifts, shifts are not limited by `end`."
  [^LocalDateTime beg ^LocalDateTime end step]
  (if (= :halfmonth (:unit step))
    (->> (range)
         (map #(.plusMonths beg (long %)))
         (take-while #(not (.isAfter ^LocalDateTime % end)))
         (mapcat (fn [t] [t (half-month-shift t)]))
         (vec))
    (->> (range)
         (map #(add-steps beg step %))
         (take-while #(not (.isAfter ^LocalDateTime % end)))
         (vec))))

(defn- next-step
  ^LocalDateTime [^LocalDateTime t step]
  (if (= :halfmonth (:unit step))
    (half-month-shift t)
    (add-steps t step 1)))

(defn- count-not-after ^long [ts ^LocalDateTime t] (count (filter #(not (.isAfter ^LocalDateTime % t)) ts)))
(defn- count-not-before ^long [ts ^LocalDateTime t] (count (filter #(not (.isBefore ^LocalDateTime % t)) ts)))

(defn- calc-steps
  "Ticks of `step` covering `[lo, hi]`: from the last point not after `lo` to the first point not before `hi`."
  [step ^LocalDateTime lo ^LocalDateTime hi]
  (let [at (step-seq (truncate lo (:start step)) hi step)
        r1 (count-not-after at lo)
        r2 (- (inc (count at)) (count-not-before at hi))
        [at r2] (if (== r2 (inc (count at)))
                  [(conj at (next-step (peek at) step)) r2]
                  [at r2])]
    (subvec at (dec r1) r2)))

(defn- closest-step-id
  "Index of the step giving number of steps closest to `n` (first one on ties)."
  ^long [^double span ^long n]
  (let [errs (map (fn [{:keys [^double secs]}] (m/abs (- (/ span secs) n))) steps)
        best (reduce min errs)]
    (long (first (keep-indexed (fn [i e] (when (== ^double e ^double best) i)) errs)))))

(defn- ensure-min-n
  "Use smaller steps (or add one second ticks at alternating ends) until there are at least `min-n` intervals."
  [ticks id lo hi min-n]
  (loop [ticks ticks id (long id) right? true]
    (cond
      (>= (dec (count ticks)) (long min-n)) [ticks id]
      (zero? id)(recur (if right?
                          (conj ticks (next-step (peek ticks) (steps 0)))
                          (into [(.minusSeconds ^LocalDateTime (first ticks) 1)] ticks))
                        id (not right?))
      :else (let [id (dec id)] (recur (calc-steps (steps id) lo hi) id right?)))))

(defn- trim-outer
  "Remove up to `dn` ticks outside `[lo, hi]`, keeping at least one on each side."
  [ticks ^long dn lo hi]
  (let [nl (dec (count-not-after ticks lo))
        nr (dec (count-not-before ticks hi))]
    (if (or (pos? nl) (pos? nr))
      (let [nc (+ nl nr)
            [^long nl ^long nr] (if (< dn nc)
                                  (let [nl (long (m/rint (/ (* dn nl) (double nc))))]
                                    [nl (- dn nl)])
                                  [nl nr])]
        (subvec ticks nl (- (count ticks) nr)))
      ticks)))

(defn- pretty-datetime
  "R's `prettyDate(c(lo, hi), n, min.n)` for `lo <= hi`, all ticks (covering the range)."
  [^LocalDateTime lo ^LocalDateTime hi ^long n ^long min-n]
  (let [d (seconds-between lo hi)
        [zlo zhi] (if (< d 1.0)
                    (let [mrg (min 30.0 (max (if (zero? d) 1.0 0.0) (/ n 2.0)))]
                      [(from-epoch-seconds (long (m/floor (- (epoch-seconds lo) mrg))))
                       (from-epoch-seconds (long (m/ceil (+ (epoch-seconds hi) mrg))))])
                    [lo hi])
        id0 (closest-step-id (seconds-between zlo zhi) n)
        [ticks ^long id] (ensure-min-n (calc-steps (steps id0) zlo zhi) id0 zlo zhi min-n)
        dn (- (dec (count ticks)) n)]
    (if (zero? dn)
      ticks
      (let [ticks (if (pos? dn) (trim-outer ticks dn lo hi) ticks)
            dn (- (dec (count ticks)) n)]
        (if (or (zero? dn)
                (and (pos? dn) (< id id0))
                (and (neg? dn) (zero? id)))
          ticks
          (let [new-id (if (pos? dn) (min (inc id) (dec (count steps))) (dec id))
                new-ticks (calc-steps (steps new-id) zlo zhi)
                new-n (dec (count new-ticks))]
            (if (and (>= new-n min-n)
                     (< (m/abs (- new-n n)) (m/abs dn)))
              new-ticks
              ticks)))))))

(defn default-min-n
  "`min.n` default of R's `prettyDate()`: `n %/% 2`."
  ^long [^long n]
  (quot n 2))

(defn pretty-datetime-ticks
  "Ticks inside `[start, end]` (any order, ticks follow the order), chosen as R's `pretty()` for date-times."
  ([start end n] (pretty-datetime-ticks start end n (default-min-n n)))
  ([^LocalDateTime start ^LocalDateTime end n min-n]
   (validate-pretty-params n min-n)
   (let [reverse? (.isBefore end start)
         [lo hi] (if reverse? [end start] [start end])
         res (filterv (fn [^LocalDateTime t] (not (or (.isBefore t lo) (.isAfter t hi))))
                      (pretty-datetime lo hi n min-n))]
     (if reverse? (vec (rseq res)) res))))

(defn pretty-datetime-nice-bounds
  "Outer pretty ticks covering `[lo, hi]` (`lo` not after `hi`): `[lo' hi' grid]`, the grid (irregular) is identified by the bounds."
  [^LocalDateTime lo ^LocalDateTime hi n min-n]
  (validate-pretty-params n min-n)
  (let [ticks (pretty-datetime lo hi n min-n)
        bounds [(first ticks) (peek ticks)]]
    (conj bounds bounds)))
