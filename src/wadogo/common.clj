(ns wadogo.common
  (:refer-clojure :exclude [range])
  (:require [fastmath.core :as m]

            [wadogo.config :as config]
            
            [wadogo.ticks.linear :as tlinear]
            [wadogo.ticks.log :as tlog]
            [wadogo.ticks.datetime :as tdatetime]
            [wadogo.ticks.pretty :as tpretty]
            [wadogo.ticks.pretty-datetime :as tpdatetime]
            [wadogo.utils :as utils]

            [wadogo.format.numbers :as fnumbers]
            [wadogo.format.datetime :as fdatetime]
            [wadogo.format.ranges :as franges])
  (:import [clojure.lang IFn]
           [java.time LocalDateTime]
           [java.time.temporal TemporalAmount]))

(def ^:private mappings {:c->c [:continuous :continuous]
                         :c->d [:continuous :discrete]
                         :d->c [:discrete :continuous]
                         :d->d [:discrete :discrete]
                         :dt->c [:datetime :continuous]})

(defonce mapping
  {:linear (mappings :c->c)
   :interpolated (mappings :c->c)
   :log (mappings :c->c)
   :symlog (mappings :c->c)
   :pow (mappings :c->c)
   :bands (mappings :d->c)
   :ordinal (mappings :d->d)
   :quantile (mappings :c->d)
   :quantize (mappings :c->d)
   :threshold (mappings :c->d)
   :histogram (mappings :c->d)
   :datetime (mappings :dt->c)
   :constant (mappings :d->d)})

(deftype ScaleType [kind domain range ticks formatter forward-fn inverse-fn data]
  Object
  (toString [_]
    (str (name kind) ": " domain " -> " range (when data (str " " data))))
  IFn
  (invoke [_ v] (forward-fn v))
  (invoke [_ v interval?] (forward-fn v interval?)))

(defn scale->map
  [^ScaleType scale]
  (assoc (.data scale)
         :kind (.kind scale)
         :domain (.domain scale)
         :range (.range scale)
         :ticks (.ticks scale)
         :formatter (.formatter scale)))

(defn strip-keys [m] (dissoc m :domain :range :kind :ticks :formatter))

;;

(defmulti scale (fn [k & _] k))

;; ticks

(defn-  kind->dispatch
  [^ScaleType scale]
  (let [kind (.kind scale)
        [d r] (mapping kind)]
    (if (and d r)
      (cond
        (= :log kind) :log
        (and (= d :continuous)
             (= r :discrete)) :discrete
        (= d :discrete) :ordinal
        (= d :datetime) :datetime
        :else :linear)
      kind)))

;; number of ticks when not given
(def ^:private ^:const default-count 10)
(def ^:private ^:const default-pretty-count 5)

(defn ticks-method
  "Tick selection method of a continuous scale (`:ticks-method` attribute): `:default` or `:pretty`."
  [^ScaleType scale]
  (let [method (or (:ticks-method (.data scale)) :default)]
    (if (#{:default :pretty} method)
      method
      (throw (ex-info (str "Unknown ticks method: " method) {:ticks-method method})))))

(defn- explicit-ticks?
  "Ticks given as a sequence of values, not a count."
  [t]
  (and (some? t) (not (number? t))))

(defn- pretty-count
  "Number of `:pretty` ticks: `t` (a non-negative integer, also as an integral double) or the default."
  ^long [t]
  (if (nil? t)
    default-pretty-count
    (let [n (if (and (number? t) (== (double t) (Math/rint (double t)))) (long t) t)]
      (tpretty/validate-pretty-params n 0)
      n)))

(defn- pretty-min-n
  "`:min-n` from `:ticks-params` or the default of the method for `n` ticks."
  [^ScaleType scale n default-min-n]
  (get-in (.data scale) [:ticks-params :min-n] (default-min-n n)))

(defmulti ticks (fn [scale] (kind->dispatch scale)))

(defn linear-ticks
  [lticks ldomain]
  (let [t (or lticks default-count)]
    (if-not (number? t) t
            (let [start (first ldomain)
                  end (last ldomain)]
              (tlinear/linear-ticks start end t)))))

(defn- continuous-ticks
  [^ScaleType scale]
  (let [t (.ticks scale)
        domain (.domain scale)]
    (if (explicit-ticks? t)
      t
      (case (ticks-method scale)
        :default (linear-ticks t domain)
        :pretty (let [n (pretty-count t)]
                  (tpretty/pretty-ticks (first domain) (last domain) n
                                        (pretty-min-n scale n tpretty/default-min-n)))))))

(defn discrete->ticks
  [tcks data]
  (let [t (or tcks data)]
    (if-not (number? t) t
            (take-nth (max 1 (m/round (/ (count data) (double t)))) data))))

(defmethod ticks :default [^ScaleType scale]
  (continuous-ticks scale))

(defmethod ticks :discrete [^ScaleType scale]
  (discrete->ticks (.ticks scale) (.range scale)))

(defmethod ticks :ordinal [^ScaleType scale]
  (discrete->ticks (.ticks scale) (.domain scale)))

(defmethod ticks :linear [^ScaleType scale]
  (continuous-ticks scale))

(defn- log-count
  "Count of default log ticks, `nil` (one tick per power) when not given or negative."
  [t]
  (when (and (some? t) (not (neg? (double t)))) t))

(defmethod ticks :log [^ScaleType scale]
  (let [t (.ticks scale)]
    (if (explicit-ticks? t)
      t
      (let [[start end] (.domain scale)
            base (:base (.data scale))]
        (case (ticks-method scale)
          :default (tlog/log-ticks start end (log-count t) base)
          :pretty (tlog/pretty-log-ticks start end (pretty-count t) base))))))

(defmethod ticks :datetime [^ScaleType scale]
  (let [t (.ticks scale)]
    (if (explicit-ticks? t)
      t
      (let [[start end] (.domain scale)]
        (case (ticks-method scale)
          :default (let [cnt (or t default-count)]
                     (if (pos? (double cnt))
                       (tdatetime/datetime-ticks start end (:millis (.data scale)) cnt)
                       []))
          :pretty (let [n (pretty-count t)]
                    (tpdatetime/pretty-datetime-ticks start end n
                                                      (pretty-min-n scale n tpdatetime/default-min-n))))))))

;; nice

(defn with-domain
  "Rebuild a scale with a new domain, keeping all other attributes."
  [^ScaleType s domain]
  (scale (.kind s) (assoc (scale->map s) :domain domain)))

(defmulti ^:private nice-bounds
  "Sorted domain `[lo hi]` extended to the nearest outer ticks of the scale: `[lo' hi' grid]`, where `grid` identifies the tick grid (a step); `nil` when not possible."
  (fn [scale _ _] (kind->dispatch scale)))

(defmethod nice-bounds :default [^ScaleType scale lo hi]
  (let [t (.ticks scale)]
    (case (ticks-method scale)
      :default (tlinear/linear-nice-bounds lo hi (or t default-count))
      :pretty (let [n (pretty-count t)]
                (tpretty/pretty-nice-bounds lo hi n (pretty-min-n scale n tpretty/default-min-n))))))

(defmethod nice-bounds :log [^ScaleType scale lo hi]
  (let [t (.ticks scale)
        base (:base (.data scale))]
    (case (ticks-method scale)
      :default (tlog/log-nice-bounds lo hi (log-count t) base)
      :pretty (tlog/pretty-log-nice-bounds lo hi (pretty-count t) base))))

(defn- datetime-unit-bounds
  "`[lo hi]` extended to the time unit of the default ticks step: `[lo' hi' unit]`."
  [^LocalDateTime lo ^LocalDateTime hi ^long cnt]
  (let [step (/ (.doubleValue (utils/datetime-diff-millis lo hi)) cnt)
        unit (utils/step->duration step)
        {:keys [truncate stepfn]} (utils/dt-data unit)
        thi (truncate hi)]
    [(truncate lo) (if (= thi hi) hi (.plus ^LocalDateTime thi ^TemporalAmount (stepfn 1))) unit]))

(defmethod nice-bounds :datetime [^ScaleType scale lo hi]
  (let [t (.ticks scale)]
    (case (ticks-method scale)
      :default (let [cnt (or t default-count)]
                 (when (pos? (double cnt)) (datetime-unit-bounds lo hi cnt)))
      :pretty (let [n (pretty-count t)]
                (tpdatetime/pretty-datetime-nice-bounds lo hi n
                                                        (pretty-min-n scale n tpdatetime/default-min-n))))))

(def ^:private discrete-dispatch #{:discrete :ordinal})
(def ^:private ^:const nice-max-iterations 10)

(defn- sorted-ends
  "Domain ends (first and last value) as `[lo hi reversed?]`, numbers as doubles."
  [domain]
  (let [a (first domain)
        b (last domain)
        [a b] (if (number? a) [(double a) (double b)] [a b])]
    (if (pos? (compare a b)) [b a true] [a b false])))

(defn- replace-ends
  [domain lo hi reversed?]
  (let [domain (vec domain)
        [a b] (if reversed? [hi lo] [lo hi])]
    (assoc domain 0 a (dec (count domain)) b)))

(defn nice
  "Extend the domain of continuous scales to the nearest outer ticks (as d3: repeated until the tick grid doesn't change, up to 10 times).

  Returns the scale unchanged for discrete domains, explicit ticks, a degenerate domain or no convergence."
  [^ScaleType scale]
  (let [domain (.domain scale)]
    (if (or (discrete-dispatch (kind->dispatch scale))
            (explicit-ticks? (.ticks scale)))
      scale
      (let [[lo hi reversed?] (sorted-ends domain)]
        (if (= lo hi)
          scale
          ;; as d3: stop when the grid calculated for the extended domain is the one it was extended with
          (loop [i (long 0) lo lo hi hi prev-grid nil]
            (let [[nlo nhi grid :as bounds] (nice-bounds scale lo hi)]
              (cond
                (nil? bounds) scale
                (= grid prev-grid) (with-domain scale (replace-ends domain lo hi reversed?))
                (>= i nice-max-iterations) scale
                :else (recur (inc i) nlo nhi grid)))))))))

;;

(defn- formatter->dispatch
  [^ScaleType scale tcks]
  (let [formatter (.formatter scale)
        kind (.kind scale)
        [d _] (mapping kind)
        stcks (remove nil? tcks)]
    (cond
      (fn? formatter) :fn
      (= d :datetime) :datetime
      (every? #(and (sequential? %)
                    (= 2 (count %))) stcks) :ranges
      (and (every? number? stcks)
           (some #(or (double? %) (float? %)) tcks)) :doubles
      (and stcks (every? int? stcks)) :ints
      :else :default)))

(defmulti formatter (fn [scale tcks] (formatter->dispatch scale tcks)))

(defmethod formatter :default [_ _] str)
(defmethod formatter :fn [^ScaleType scale _] (.formatter scale))
(defmethod formatter :datetime [_ tcks] (fdatetime/time-format tcks))
(defmethod formatter :ints [^ScaleType scale _] (fnumbers/int-formatter (:formatter-params (.data scale))))
(defmethod formatter :doubles [^ScaleType scale _] (fnumbers/formatter (:formatter-params (.data scale))))
(defmethod formatter :ranges [^ScaleType scale _] (franges/range-formatter {:formatter-params (.data scale)}))

;; params

(defn merge-params
  [scale-key params]
  (merge (config/default-params scale-key) params))

(defn log-params
  [{:keys [domain base] :as params}]
  (let [[x1 x2] domain]
    (if (and base (not x2))
      (assoc params :domain [(or x1 1.0) base])
      params)))
