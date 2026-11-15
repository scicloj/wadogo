(ns wadogo.common
  "Internal core of scales: the scale type, the constructor multimethod extended by every kind, shared attributes (`:clamp`, `:unknown`, `:round`), ticks, `nice` and formatter selection.

  The public API is `wadogo.scale`; vars here are internal (`:no-doc`), except the ones used by `wadogo.scale/defcustom` expansions, which stay resolvable."
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
  (:import [clojure.lang IFn ArityException]
           [java.time LocalDateTime]
           [java.time.temporal TemporalAmount]))

(def ^:private mappings {:c->c [:continuous :continuous]
                         :c->d [:continuous :discrete]
                         :d->c [:discrete :continuous]
                         :d->d [:discrete :discrete]
                         :dt->c [:datetime :continuous]})

(def ^{:no-doc true
       :doc "Internal. Map from a scale kind to the pair `[domain-type range-type]`, each `:continuous`, `:discrete` or `:datetime`. Custom kinds created by `defcustom` are not listed."}
  mapping
  {:linear (mappings :c->c)
   :identity (mappings :c->c)
   :radial (mappings :c->c)
   :interpolated (mappings :c->c)
   :log (mappings :c->c)
   :symlog (mappings :c->c)
   :pow (mappings :c->c)
   :sequential (mappings :c->c)
   :diverging (mappings :c->c)
   :bands (mappings :d->c)
   :point (mappings :d->c)
   :ordinal (mappings :d->d)
   :quantile (mappings :c->d)
   :quantize (mappings :c->d)
   :threshold (mappings :c->d)
   :histogram (mappings :c->d)
   :datetime (mappings :dt->c)
   :constant (mappings :d->d)})

(deftype ^:no-doc ScaleType [kind domain range ticks formatter forward-fn inverse-fn data]
  Object
  (toString [_]
    (str (name kind) ": " domain " -> " range (when data (str " " data))))
  IFn
  (invoke [_ v] (forward-fn v))
  (invoke [_ v interval?] (forward-fn v interval?))
  (applyTo [this args]
    (let [n (long (bounded-count 3 args))]
      (case n
        1 (.invoke this (first args))
        2 (.invoke this (first args) (second args))
        (throw (ArityException. n (str "wadogo scale " (name kind))))))))

(alter-meta! #'->ScaleType assoc :no-doc true
             :doc "Internal. Positional constructor of a scale: kind, domain, range, ticks, formatter, forward function, inverse function and the map of other attributes.")

(defn ^:no-doc scale->map
  "Internal. All attributes of `scale` as one map: the other attributes merged with `:kind`, `:domain`, `:range`, `:ticks` and `:formatter`. Building a scale of the same kind from this map gives an equal scale."
  [^ScaleType scale]
  (assoc (.data scale)
         :kind (.kind scale)
         :domain (.domain scale)
         :range (.range scale)
         :ticks (.ticks scale)
         :formatter (.formatter scale)))

(defn ^:no-doc strip-keys
  "Internal. Attribute map `m` without the keys stored in dedicated scale fields (`:domain`, `:range`, `:kind`, `:ticks`, `:formatter`)."
  [m] (dissoc m :domain :range :kind :ticks :formatter))

;;

(defmulti ^:no-doc scale
  "Internal. Scale constructor dispatched on the kind keyword, called with the kind and an optional attribute map. Every kind namespace adds a method; `defcustom` adds one for a custom kind."
  (fn [k & _] k))

;; shared attributes: :unknown, :clamp, :round

(defn ^:no-doc missing-value?
  "Internal. `true` for `nil` and `NaN`, the inputs mapped to the `:unknown` attribute."
  [v]
  (or (nil? v)
      (and (number? v) (m/nan? (double v)))))

(defn ^:no-doc js-round
  "Internal. Number `x` rounded half up (JavaScript `Math.round`, as d3 `rangeRound`), as a double."
  ^double [^double x]
  (m/floor (+ x 0.5)))

(defn- clamper
  "Function constraining a value to the closed interval between the smallest and the largest of `xs`, compared with `compare`."
  [xs]
  (let [lo (reduce (fn [a b] (if (pos? (compare a b)) b a)) xs)
        hi (reduce (fn [a b] (if (neg? (compare a b)) b a)) xs)]
    (if (number? lo)
      (let [lo (double lo) hi (double hi)]
        (fn [v] (m/constrain (double v) lo hi)))
      (fn [v] (cond
                (neg? (compare v lo)) lo
                (pos? (compare v hi)) hi
                :else v)))))

(defn ^:no-doc continuous-scale
  "Internal. A scale of a kind with a continuous (numerical or datetime) domain and a numerical range, with the shared attributes applied to `forward` and `inverse`.

  Parameters:

  - `kind`, `domain`, `range`: the scale fields; the clamping interval is spanned by the smallest and the largest value of `domain`.
  - `params`: the attribute map, read for `:ticks`, `:formatter`, `:clamp`, `:unknown` and `:round`.
  - `forward`, `inverse`: the plain mappings between domain and range.
  - `data`: the attributes kept in the scale (see [[strip-keys]]).

  - `coerce` (optional): function converting a forward input to the domain type before clamping.

  The forward function returns `:unknown` for `nil` and `NaN`, constrains the input to the domain when `:clamp` is truthy and rounds the result half up when `:round` is truthy. The inverse function constrains its result to the domain when `:clamp` is truthy."
  ([kind domain range params forward inverse data]
   (continuous-scale kind domain range params forward inverse data nil))
  ([kind domain range params forward inverse data coerce]
  (let [{:keys [clamp unknown round]} params
        dclamp (when clamp (cond-> (clamper domain) coerce (comp coerce)))
        fwd (cond
              (and dclamp round) (fn [v] (js-round (forward (dclamp v))))
              dclamp (fn [v] (forward (dclamp v)))
              round (fn [v] (js-round (forward v)))
              :else forward)
        inv (if dclamp (fn [v] (dclamp (inverse v))) inverse)]
    (ScaleType. kind domain range (:ticks params) (:formatter params)
                (fn [v] (if (missing-value? v) unknown (fwd v)))
                inv
                data))))

(defn ^:no-doc interval-forward
  "Internal. Forward function of a kind mapping numbers to intervals (continuous domain, discrete range).

  `step-fn` maps a number to an interval index, `intervals` is the vector of interval maps (each with the range value under `:value`). The function of one argument returns the `:value` of the interval of a number, with a second truthy argument the whole interval map; `unknown` is returned for `nil` and `NaN`."
  [step-fn intervals unknown]
  (fn interval-forward-fn
    ([v] (interval-forward-fn v false))
    ([v interval?]
     (if (missing-value? v)
       unknown
       (let [res (intervals (step-fn (double v)))]
         (if interval? res (:value res)))))))

;; ticks

(def ^:private transformed-kinds #{:sequential :diverging})

(defn- kind->dispatch
  "Ticks and `nice` method of a scale: `:log`, `:linear`, `:discrete` (continuous domain, discrete range), `:ordinal` (discrete domain), `:datetime`, or the kind of a custom scale."
  [^ScaleType scale]
  (let [kind (.kind scale)
        [d r] (mapping kind)]
    (if (and d r)
      (cond
        (or (= :log kind)
            (and (transformed-kinds kind)
                 (= :log (:transform (.data scale))))) :log
        (and (= d :continuous)
             (= r :discrete)) :discrete
        (= d :discrete) :ordinal
        (= d :datetime) :datetime
        :else :linear)
      kind)))

;; number of ticks when not given
(def ^:private ^:const default-count 10)
(def ^:private ^:const default-pretty-count 5)

(defn- ticks-method
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

(defn- calendar-unit
  "`:unit` of calendar ticks from `:ticks-params` (`:datetime` with `:pretty`), or `nil`."
  [^ScaleType scale]
  (get-in (.data scale) [:ticks-params :unit]))

(defn- calendar-step-count
  "`:step` (units per tick) of calendar ticks from `:ticks-params`, default 1."
  [^ScaleType scale]
  (get-in (.data scale) [:ticks-params :step] 1))

(defn- check-no-calendar!
  "Throw when a `:datetime` scale with `:ticks-method :default` has calendar `:ticks-params` (they need `:pretty`)."
  [^ScaleType scale]
  (when (calendar-unit scale)
    (throw (ex-info "Calendar ticks (`:ticks-params` `:unit`) need `:ticks-method :pretty`."
                    {:ticks-params (:ticks-params (.data scale))}))))

(defmulti ^:no-doc ticks
  "Internal. Ticks of a scale, dispatched on the kind group (see `wadogo.scale/ticks`)."
  (fn [scale] (kind->dispatch scale)))

(defn- linear-ticks
  "Explicit ticks `lticks`, or d3 style ticks of `ldomain` (first and last value) for a count `lticks` (default 10)."
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

(defn- discrete->ticks
  "Explicit ticks `tcks`, all values of `data` (`tcks` is `nil`), or about `tcks` values of `data` taken at a regular step."
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

(def ^:private ^:const default-log-base 10.0)

(defmethod ticks :log [^ScaleType scale]
  (let [t (.ticks scale)]
    (if (explicit-ticks? t)
      t
      (let [domain (.domain scale)
            start (first domain)
            end (last domain)
            base (:base (.data scale) default-log-base)]
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
                     (check-no-calendar! scale)
                     (if (pos? (double cnt))
                       (if (pos? (compare start end))
                         (reverse (tdatetime/datetime-ticks end start (.abs ^java.math.BigDecimal (:millis (.data scale))) cnt))
                         (tdatetime/datetime-ticks start end (:millis (.data scale)) cnt))
                       []))
          :pretty (if-let [unit (calendar-unit scale)]
                    (tpdatetime/calendar-ticks start end unit (calendar-step-count scale))
                    (let [n (pretty-count t)]
                      (tpdatetime/pretty-datetime-ticks start end n
                                                        (pretty-min-n scale n tpdatetime/default-min-n)))))))))

;; nice

(defn ^:no-doc with-domain
  "Internal. The scale `s` rebuilt by its kind constructor with a new `domain`, keeping all other attributes."
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
        base (:base (.data scale) default-log-base)]
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
                 (check-no-calendar! scale)
                 (when (pos? (double cnt)) (datetime-unit-bounds lo hi cnt)))
      :pretty (if-let [unit (calendar-unit scale)]
                (tpdatetime/calendar-nice-bounds lo hi unit (calendar-step-count scale))
                (let [n (pretty-count t)]
                  (tpdatetime/pretty-datetime-nice-bounds lo hi n
                                                          (pretty-min-n scale n tpdatetime/default-min-n)))))))

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

(defn ^:no-doc nice
  "Internal. `scale` with the domain extended to the nearest outer ticks (as d3: repeated until the tick grid doesn't change, up to 10 times); see `wadogo.scale/nice`.

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

(defmulti ^:no-doc formatter
  "Internal. Formatter of `scale` for the values `tcks`: the `:formatter` function, a datetime formatter, a range formatter for interval pairs, a number formatter for doubles or integers, or `str`."
  (fn [scale tcks] (formatter->dispatch scale tcks)))

(defmethod formatter :default [_ _] str)
(defmethod formatter :fn [^ScaleType scale _] (.formatter scale))
(defmethod formatter :datetime [^ScaleType scale tcks] (fdatetime/time-format tcks (.domain scale)))
(defmethod formatter :ints [^ScaleType scale _] (fnumbers/int-formatter (:formatter-params (.data scale))))
(defmethod formatter :doubles [^ScaleType scale _] (fnumbers/formatter (:formatter-params (.data scale))))
(defmethod formatter :ranges [^ScaleType scale _] (franges/range-formatter (:formatter-params (.data scale))))

;; params

(defn ^:no-doc merge-params
  "Internal. Attributes `params` over the defaults of the kind `scale-key` (see `wadogo.config/default-params`)."
  [scale-key params]
  (merge (config/default-params scale-key) params))

(defn ^:no-doc log-params
  "Internal. `params` with a one-value `:domain` `[x]` completed to `[x base]` when `:base` is given (`[1.0 base]` for an empty domain)."
  [{:keys [domain base] :as params}]
  (let [[x1 x2] domain]
    (if (and base (not x2))
      (assoc params :domain [(or x1 1.0) base])
      params)))
