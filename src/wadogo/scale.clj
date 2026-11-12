(ns wadogo.scale
  "Scales: functions mapping values of a domain to values of a range, with inverses, ticks and tick formatting, modelled on d3-scale.

  A scale is created by [[scale]] from a kind keyword and a map of attributes, is immutable and is called as a function of a domain value (some kinds accept a second argument, see below). The other functions read a scale ([[domain]], [[range]], [[ticks]], [[format]], [[inverse]]) or build a changed copy ([[with-domain]], [[with-range]], [[nice]], ...).

  Kinds (`[domain range]` types; [[kind-info]] returns the full description of a kind):

  - `:linear` (continuous, continuous): linear mapping.
  - `:identity` (continuous, continuous): the value itself, the domain is the range.
  - `:pow` (continuous, continuous): power mapping, `:exponent` (default `0.5`).
  - `:log` (continuous, continuous): logarithmic mapping of a domain of one sign.
  - `:symlog` (continuous, continuous): bi-symmetric logarithmic mapping, constant `:C`.
  - `:radial` (continuous, continuous): range values are radii of areas linear in the domain.
  - `:interpolated` (continuous, continuous): piecewise interpolation through `(domain, range)` pairs.
  - `:sequential` (continuous, continuous): domain normalised to `[0, 1]` and passed to an `:interpolator` (gradients).
  - `:diverging` (continuous, continuous): domain `[low mid high]` normalised to `[0, 0.5, 1]` and passed to an `:interpolator`.
  - `:datetime` (datetime, continuous): linear mapping of time.
  - `:quantize` (continuous, discrete): equal intervals of the domain.
  - `:quantile` (continuous, discrete): intervals between quantiles of a data sample.
  - `:threshold` (continuous, discrete): intervals between given thresholds.
  - `:histogram` (continuous, discrete): histogram bins of a data sample.
  - `:bands` (discrete, continuous): evenly distributed bands of the range.
  - `:point` (discrete, continuous): evenly spaced points of the range.
  - `:ordinal` (discrete, discrete): values paired by position, the range cycled.
  - `:constant` (discrete, discrete): the range for every value.

  Attributes shared by kinds: `:domain`, `:range`, `:ticks`, `:ticks-method`, `:ticks-params`, `:formatter`, `:formatter-params` (see [[scale]]), and `:clamp`, `:unknown`, `:round` for continuous ranges. Other attributes are kind specific and are returned by [[data]]. New continuous kinds can be defined with [[defcustom]]."
  (:refer-clojure :exclude [range format])
  (:require [wadogo.common :as common]
            [wadogo.utils :refer [->extent make-norm build-forward build-inverse]]
            
            [wadogo.scale.linear]
            [wadogo.scale.identity]
            [wadogo.scale.interpolated]
            [wadogo.scale.log]
            [wadogo.scale.symlog]
            [wadogo.scale.pow]
            [wadogo.scale.radial]
            [wadogo.scale.sequential]
            [wadogo.scale.diverging]
            [wadogo.scale.bands]
            [wadogo.scale.point]
            [wadogo.scale.ordinal]
            [wadogo.scale.quantile]
            [wadogo.scale.quantize]
            [wadogo.scale.threshold]
            [wadogo.scale.histogram]
            [wadogo.scale.datetime]
            [wadogo.scale.constant]
            
            [fastmath.stats :as stats]
            [fastmath.core :as m])
  (:import [wadogo.common ScaleType]
           [java.time Duration LocalDateTime]))

(def ^{:doc "Sorted list of the scale kinds (keywords) defined when `wadogo.scale` was loaded. Kinds defined later by [[defcustom]] are not included. See [[kind-info]] for a description of each kind."}
  scale-kinds (-> common/scale methods keys sort))

(defn scale?
  "Checks whether `scale` is a scale object created by [[scale]].

  Returns `true` for a scale and `false` for any other value, `nil` included."
  [scale]
  (instance? ScaleType scale))

(defn scale
  "Creates a scale of a given kind.

  Parameters:

  - `scale-kind`: a keyword from [[scale-kinds]] or a kind defined by [[defcustom]].
  - `attributes` (optional): a map of attributes; missing attributes take the kind's defaults (`wadogo.config/default-params`). Every kind except `:quantile` and `:histogram` (which need a `:domain`, a data sample) can be created without attributes.

  Attributes common to all kinds:

  - `:domain`, `:range`: the domain and the range; their form depends on the kind, see [[kind-info]].
  - `:ticks`: the number of ticks or a sequence of ticks (used as given).
  - `:ticks-method`: how ticks are chosen for continuous and datetime domains (ignored by other kinds):
      - `:default` (default): d3 style ticks, 10 ticks by default (log: one tick per power).
      - `:pretty`: R's `pretty()` ticks inside the domain (log: subdivisions of powers of the base or powers, the grid with the number of ticks closest to the requested one), 5 ticks by default.
  - `:ticks-params`: parameters of `:pretty` ticks, a map with `:min-n`, the minimal number of intervals (default: `n/3` for numbers, `n/2` for datetime; not used by log scales).
  - `:formatter`: a function converting a value to a string, used by [[format]] instead of the default formatter.
  - `:formatter-params`: parameters of the default number formatter (see `wadogo.format.numbers/formatter`) and of the range formatter (see `wadogo.format.ranges/range-formatter`).

  Attributes of kinds with a continuous range (`:linear`, `:identity`, `:pow`, `:log`, `:symlog`, `:radial`, `:interpolated`, `:sequential`, `:diverging`, `:datetime`):

  - `:clamp` (default `false`): when truthy, values outside the domain are constrained to it, and [[inverse]] returns values inside the domain.
  - `:unknown` (default `nil`): returned for `nil` and `NaN` input (and for values outside the domain of `:bands`, `:point` and `:ordinal`; `nil` and `NaN` for `:quantize`, `:quantile`, `:threshold` and `:histogram`).
  - `:round` (default `false`): when truthy, results are rounded half up (d3 `rangeRound`; `:bands` and `:point` round their layout).

  Other attributes are kind specific, see [[kind-info]]; all attributes except the first five are returned by [[data]].

  Returns a scale, a function of a domain value returning a range value. `:quantize`, `:quantile`, `:threshold`, `:histogram`, `:bands` and `:point` accept a second argument: when truthy, the whole interval or band map is returned. `:ordinal` and `:constant` accept and ignore it; other kinds take one argument.

  Throws an `AssertionError` for `:quantile` and `:histogram` without a domain, an `IllegalArgumentException` for an unknown kind, and `ex-info` for invalid kind specific attributes (see [[kind-info]]).

  See also [[nice]], [[->map]], [[with-data]]."
  ([scale-kind] (common/scale scale-kind))
  ([scale-kind attributes] (common/scale scale-kind attributes)))

(defn kind-info
  "Returns the description of a scale kind: its attributes with defaults, forward and inverse mapping, ticks and edge cases.

  Parameters:

  - `kind`: a keyword from [[scale-kinds]], or a kind defined by [[defcustom]].

  Returns a string (the docstring of the kind's namespace, as `wadogo.scale.linear`); for a kind defined by [[defcustom]] a short generic description.

  Throws `ex-info` with `{:kind kind}` for an unknown kind."
  [kind]
  (let [kind-ns (when (keyword? kind) (find-ns (symbol (str "wadogo.scale." (name kind)))))]
    (cond
      (and kind-ns (contains? (methods common/scale) kind)) (:doc (meta kind-ns))
      (contains? (methods common/scale) kind) (str "Custom continuous numerical scale `" kind "` defined by `wadogo.scale/defcustom`, with the attributes of the `:linear` kind.")
      :else (throw (ex-info (str "Unknown scale kind: " kind) {:kind kind})))))

(defn ->map
  "Converts a scale to a map of all its attributes.

  Returns a map with `:kind`, `:domain`, `:range`, `:ticks` and `:formatter` merged with the other attributes (see [[data]]). Kinds store computed values among the other attributes too (as `:bands` or `:quantiles`); building a scale of the same kind from the map gives an equal scale.

  See also [[scale]], [[with-data]]."
  [scale]
  (common/scale->map scale))

(defn forward
  "Maps a domain value `v` with `scale`, the same as calling `(scale v)`.

  Returns the range value; see [[kind-info]] for the mapping of each kind, including values outside the domain, `nil` and `NaN`.

  See also [[inverse]]."
  [scale v] (scale v))

(defn inverse
  "Maps a range value `v` back to the domain with `scale`.

  Returns a domain value for continuous domains; for kinds with a continuous domain and a discrete range (`:quantize`, `:quantile`, `:threshold`, `:histogram`) the interval map of a range value; for `:bands` and `:point` the domain value of the band or point at a number; for `:ordinal` the first domain value mapped to a range value. Values without an inverse give `nil` or the `:unknown` attribute, see [[kind-info]].

  See also [[forward]]."
  [^ScaleType scale v] ((.inverse-fn scale) v))

(defn domain
  "Returns the domain of `scale`, as stored after parsing the `:domain` attribute (for example two values of a continuous domain, the data sample of `:quantile` and `:histogram`, or `LocalDateTime` values of `:datetime`).

  See also [[range]], [[with-domain]], [[size]]."
  [^ScaleType scale] (.domain scale))

(defn range
  "Returns the range of `scale`, as stored after parsing the `:range` attribute (for example two numbers of a continuous range or the vector of range values of a discrete range).

  See also [[domain]], [[with-range]], [[size]]."
  [^ScaleType scale] (.range scale))

(defn kind
  "Returns the kind of `scale`, a keyword as `:linear`.

  See also [[scale-kinds]], [[kind-info]], [[with-kind]]."
  [^ScaleType scale] (.kind scale))

(declare with-ticks)

(defn ticks
  "Returns the ticks of `scale`: chosen values, usually evenly distributed through the domain (continuous domains) or the domain or range values (discrete kinds).

  Parameters:

  - `scale`: a scale.
  - `proposed-ticks` (optional): the number of ticks or a sequence of ticks, replacing the `:ticks` attribute (see [[with-ticks]]).

  Ticks depend on the kind:

  - continuous domains: d3 style ticks (`:ticks-method :default`, 10 by default) or R `pretty()` ticks (`:pretty`, 5 by default) inside the domain, in the domain's order; the count is approximate; `:log` uses powers of the base and their subdivisions.
  - `:datetime`: datetimes spaced by a whole number of time units (`:default`) or R `pretty()` date-times (`:pretty`).
  - discrete domains (`:bands`, `:point`, `:ordinal`, `:constant`): the domain values.
  - discrete ranges (`:quantize`, `:quantile`, `:threshold`, `:histogram`): the range values.

  For discrete kinds a number `n` of ticks takes every `k`-th value with `k` close to `count/n`. Explicit ticks are returned unchanged. A tick count of zero or less gives no ticks (log `:default`: one tick per power).

  See also [[format]], [[nice]]."
  ([^ScaleType scale] (common/ticks scale))
  ([^ScaleType scale proposed-ticks] (ticks (with-ticks scale proposed-ticks))))

(defn format
  "Formats values of `scale` as strings.

  Parameters:

  - `scale`: a scale.
  - `vs` (optional, default: [[ticks]] of the scale): the values to format.

  Returns a sequence of strings, made by the function returned by [[formatter]] for `vs`.

  See also [[formatter]], [[with-formatter]]."
  ([scale]
   (format scale (ticks scale)))
  ([scale vs]
   (map (common/formatter scale vs) vs)))

(defn formatter
  "Returns the formatter of `scale` for the values `vs` (default: [[ticks]] of the scale), a function converting one value to a string.

  The formatter is the `:formatter` attribute when it's a function. Otherwise it's chosen by the values: a datetime formatter for the `:datetime` kind (pattern by the step between values, see `wadogo.format.datetime/time-format`), a range formatter for interval pairs, a number formatter for doubles or for integers, and `str` for other values; number and range formatters take the `:formatter-params` attribute.

  See also [[format]], [[with-formatter]]."
  ([scale]
   (formatter scale (ticks scale)))
  ([scale vs]
   (common/formatter scale vs)))

(defn data
  "Returns the attributes of `scale` other than `:domain`, `:range`, `:kind`, `:ticks` and `:formatter`.

  Parameters:

  - `scale`: a scale.
  - `key` (optional): an attribute name; only its value is returned.

  Kinds store computed values here too, as `:bands`, `:bandwidth` and `:step` (`:bands`, `:point`), `:quantiles` (`:quantile`), `:thresholds` (`:quantize`), `:bins` (`:histogram`) or `:millis` (`:datetime`).

  See also [[->map]], [[with-data]]."
  ([^ScaleType scale]
   (.data scale))
  ([^ScaleType scale key]
   (get (.data scale) key)))

(defn with-domain
  "Returns `scale` rebuilt with a new `domain`, keeping all other attributes.

  See also [[update-domain]], [[with-range]], [[nice]]."
  [scale domain]
  (common/with-domain scale domain))

(defn nice
  "Extends the domain to the nearest ticks outside it, so the domain ends are ticks (except `:default` datetime ticks, which never contain the domain ends, the domain is extended to whole time units there).

  Ticks are calculated as by [[ticks]]: with the scale's `:ticks` count and `:ticks-method`. The extension is repeated (up to 10 times) while the tick step changes, as in d3.

  - `:linear`, `:identity`, `:pow`, `:symlog`, `:radial`, `:interpolated` (first and last domain value), `:sequential`, `:diverging` (first and last domain value) and custom scales: multiples of the tick step.
  - `:log` (and the `:log` transform of `:sequential` and `:diverging`): the nearest outer tick (powers of the base or, for `:pretty`, their subdivisions); when linear ticks are used (too few powers inside) and the outer tick would be zero or less, the lower end is the largest power of the base below the domain instead.
  - `:datetime`: the time unit of the tick step (`:default`), the outer ticks (`:pretty`).

  The scale is returned unchanged for discrete domains or ranges (`:bands`, `:point`, `:ordinal`, `:quantile`, `:quantize`, `:threshold`, `:histogram`, `:constant`), an explicit sequence of ticks, a domain with equal ends, or when the extension doesn't converge.

  With options `{:max-extension f :max-count n}` the extension is limited: tick counts from the scale's own count
  (default 10, or 5 for `:pretty`) up to `n` (default 50) are tried, and the first extension that moves the
  domain ends by at most `f` of the range span in total (measured with the unclamped scale) is used; the scale
  is returned unchanged when none qualifies or the range span is zero. The scale keeps its own `:ticks` (only the domain changes), so with
  a larger count the domain ends needn't be ticks of the scale. Use it when few ticks would round a domain to a
  much larger one (e.g. `[0.3 8.2]` to `[-10 10]` with 2 ticks).

  See also [[with-domain]]."
  ([scale]
   (common/nice scale))
  ([scale {:keys [max-extension max-count] :or {max-count 50}}]
   (when-not (and (number? max-extension) (>= (double max-extension) 0.0))
     (throw (ex-info "nice :max-extension must be a non-negative number." {:max-extension max-extension})))
   (let [t (:ticks (common/scale->map scale))
         start (cond (number? t) (long t)
                     (= :pretty (:ticks-method (data scale))) 5
                     :else 10)]
     (when-not (and (integer? max-count) (pos? (long max-count)))
       (throw (ex-info "nice :max-count must be a positive integer." {:max-count max-count})))
     (if (or (and (some? t) (not (number? t))) (< (long max-count) start)
             (let [[r0 r1] (->extent (range scale))] (== (double r0) (double r1))))
       scale
       (let [unclamped (common/scale (kind scale) (assoc (common/scale->map scale) :clamp false))
             [r0 r1] (->extent (range scale))
             span (Math/abs (- (double r1) (double r0)))
             d0 (first (domain scale))
             d1 (last (domain scale))
             extension (fn [niced]
                         (let [n0 (first (domain niced)) n1 (last (domain niced))]
                           (if (zero? span)
                             0.0
                             (/ (+ (Math/abs (- (double (unclamped n0)) (double (unclamped d0))))
                                   (Math/abs (- (double (unclamped n1)) (double (unclamped d1)))))
                                span))))]
         (or (some (fn [c]
                     (let [niced (common/nice (with-ticks scale c))]
                       (when (<= (double (extension niced)) (double max-extension))
                         (with-domain scale (domain niced)))))
                   (clojure.core/range start (inc (long max-count))))
             scale))))))

(defn update-domain
  "Returns `scale` rebuilt with the domain replaced by `(f domain)`, keeping all other attributes.

  See also [[with-domain]]."
  [^ScaleType scale f]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :domain (f (.domain scale)))))

(defn with-range
  "Returns `scale` rebuilt with a new `range`, keeping all other attributes.

  The range of `:identity`, `:sequential` and `:diverging` isn't an attribute (it follows the domain or the interpolator), so it isn't changed.

  See also [[update-range]], [[with-domain]]."
  [^ScaleType scale range]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :range range)))

(defn update-range
  "Returns `scale` rebuilt with the range replaced by `(f range)`, keeping all other attributes.

  See also [[with-range]]."
  [^ScaleType scale f]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :range (f (.range scale)))))

(defn with-kind
  "Returns a scale of another `kind` built from all attributes of `scale`.

  Attributes the new kind doesn't use are kept in [[data]]; the stored domain and range must be valid for the new kind.

  See also [[kind]], [[scale]]."
  [^ScaleType scale kind]
  (common/scale kind (assoc (common/scale->map scale) :kind kind)))

(defn with-data
  "Returns `scale` rebuilt with attributes merged into its attributes (any attribute, also `:domain` or `:range`).

  Parameters:

  - `scale`: a scale.
  - `data`: a map of attributes, or
  - `k`, `v`, `kvs`: attribute names and values, as in `assoc`.

  See also [[data]], [[->map]]."
  ([^ScaleType scale data]
   (common/scale (.kind scale) (merge (common/scale->map scale) data)))
  ([^ScaleType scale k v & kvs]
   (common/scale (.kind scale) (apply assoc (common/scale->map scale) k v kvs))))

(defn with-ticks
  "Returns `scale` rebuilt with the `:ticks` attribute: a number of ticks or a sequence of ticks.

  See also [[ticks]]."
  [^ScaleType scale ticks]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :ticks ticks)))

(defn with-formatter
  "Returns `scale` rebuilt with the `:formatter` attribute `fmt`, a function converting a value to a string (or `nil` for the default formatter).

  See also [[format]], [[formatter]]."
  [^ScaleType scale fmt]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :formatter fmt)))

(def ^{:doc "Map from every scale kind to the pair `[domain-type range-type]`, each `:continuous`, `:discrete` or `:datetime`. Kinds defined by [[defcustom]] are not listed. See [[size]]."}
  mapping common/mapping)

(defn- general-size
  [data typ]
  (case typ
    :discrete (if (sequential? data) (count data) 1)
    :continuous (let [a (first data) b (last data)]
                  (when (and (number? a) (number? b))
                    (m/abs (- (double b) (double a)))))
    :datetime (.abs (Duration/between ^LocalDateTime (first data) ^LocalDateTime (last data)))
    nil))

(defn size
  "Returns the size of the range (default) or the domain of `scale`.

  Parameters:

  - `scale`: a scale.
  - `range-or-domain` (optional, default `:range`): `:domain` for the domain, any other value for the range.

  Returns, by the type in [[mapping]]:

  - discrete: the number of values (`1` for a single value);
  - continuous: the absolute difference between the last and the first value, `nil` when they aren't numbers (interpolator results of `:sequential` and `:diverging`);
  - datetime: the absolute `java.time.Duration` between the first and the last value.

  Returns `nil` for kinds not in [[mapping]] (custom kinds)."
  ([scale] (size scale :range))
  ([scale range-or-domain]
   (let [[dtype rtype] (-> scale kind mapping)]
     (if (= range-or-domain :domain)
       (general-size (domain scale) dtype)
       (general-size (range scale) rtype)))))

(defn extent
  "Returns the pair `[min max]` of a sequence of numbers `xs`.

  A helper for building domains from data, see `fastmath.stats/extent`."
  [xs]
  (let [[minx maxx] (stats/extent xs)]
    [minx maxx]))

(defmacro defcustom
  "Defines a custom continuous numerical scale kind from a transform and its inverse.

  Parameters:

  - `scale-name`: the kind keyword, as `:sqrt-custom`.
  - `forward`: a function of a double, the transform (as `m/sqrt`).
  - `inverse`: a function of a double, the inverse transform (as `m/sq`).

  The scale maps the transformed domain linearly to the range, as the `:pow` kind does with a power. Attributes: `:domain` and `:range` (default `[EPSILON 1.0]` each, two numbers or longer sequences reduced to the minimum and maximum), `:ticks`, `:ticks-method`, `:formatter`, `:clamp`, `:unknown`, `:round`. Ticks and [[nice]] are linear in the domain.

  The kind isn't added to [[scale-kinds]] or [[mapping]]; [[kind-info]] returns a generic description."
  [scale-name forward inverse]
  `(defmethod common/scale ~scale-name
     ([~'_] (common/scale ~scale-name {}))
     ([~'_ ~'params]
      (let [params# (merge {:domain [m/EPSILON 1.0]
                            :range [m/EPSILON 1.0]} ~'params)
            [dstart# dend# :as d#] (->extent (:domain params#))
            [rstart# rend# :as r#] (->extent (:range params#))
            start# (~forward dstart#)
            end# (~forward dend#)
            fnorm# (make-norm start# end# rstart# rend#)
            inorm# (make-norm rstart# rend# start# end#)]
        (common/continuous-scale ~scale-name d# r# params#
                                 (build-forward ~forward fnorm#)
                                 (build-inverse ~inverse inorm#)
                                 (common/strip-keys params#))))))


(comment

  (def my-scale (scale :linear {:domain [-1 2] :range [100 200]}))
  my-scale
  ;; => #object[wadogo.common.ScaleType 0x13f0e474 "linear: [-1 2] -> [100 200] {}"]

  (my-scale 0) ;; => 133.33333333333331
  (forward my-scale 0) ;; => 133.33333333333331
  (inverse my-scale 150) ;; => 0.5
  (domain my-scale) ;; => [-1 2]
  (range my-scale) ;; => [100 200]
  (kind my-scale) ;; => :linear

  (with-domain my-scale [200 300])
  ;; => #object[wadogo.common.ScaleType 0x36a87e4d "linear: [200 300] -> [100 200] {}"]
  (with-range my-scale [-10 -20])
  ;; => #object[wadogo.common.ScaleType 0xae5f779 "linear: [-1 2] -> [-10 -20] {}"]

  (ticks my-scale)
  ;; => (-1.0 -0.8 -0.6 -0.4 -0.2 -0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0)
  (format my-scale)
  ;; => ("-1.0" "-0.8" "-0.6" "-0.4" "-0.2" "0.0" "0.2" "0.4" "0.6" "0.8" "1.0" "1.2" "1.4" "1.6" "1.8" "2.0")

  (def dt1 (java.time.LocalDateTime/of 2012 5 5 10 10 10))
  (def dt2 (java.time.LocalDateTime/of 2012 12 5 10 10 10))
  (def dt-scale (scale :datetime {:domain [dt1 dt2]}))
  dt-scale
  ;; => #object[wadogo.common.ScaleType 0xe0933e7 "datetime: [#object[java.time.LocalDateTime 0x75e397a \"2012-05-05T10:10:10\"] #object[java.time.LocalDateTime 0xba90eb6 \"2012-12-05T10:10:10\"]] -> [0.0 1.0] {:millis 18489600000.0M}"]

  (dt-scale (java.time.LocalDateTime/of 2012 7 5 10 10 10))
  ;; => 0.2850467289719626
  (inverse dt-scale 0.5)
  ;; => #object[java.time.LocalDateTime 0x455fe98 "2012-08-20T10:10:10"]

  (ticks dt-scale)
  ;; => [#object[java.time.LocalDateTime 0x37c790bb "2012-05-06T00:00"]
  ;;     #object[java.time.LocalDateTime 0x788625be "2012-05-27T00:00"]
  ;;     #object[java.time.LocalDateTime 0x2b6cf36a "2012-06-17T00:00"]
  ;;     #object[java.time.LocalDateTime 0x32d036d6 "2012-07-08T00:00"]
  ;;     #object[java.time.LocalDateTime 0x686e204e "2012-07-29T00:00"]
  ;;     #object[java.time.LocalDateTime 0x218c4328 "2012-08-19T00:00"]
  ;;     #object[java.time.LocalDateTime 0x6d3703d0 "2012-09-09T00:00"]
  ;;     #object[java.time.LocalDateTime 0x5ee2fe26 "2012-09-30T00:00"]
  ;;     #object[java.time.LocalDateTime 0x4717c9e4 "2012-10-21T00:00"]
  ;;     #object[java.time.LocalDateTime 0x441472d "2012-11-11T00:00"]
  ;;     #object[java.time.LocalDateTime 0x513dedf6 "2012-12-02T00:00"]]

  (format dt-scale)
  ;; => ("May-06" "May-27" "Jun-17" "Jul-08" "Jul-29" "Aug-19" "Sep-09" "Sep-30" "Oct-21" "Nov-11" "Dec-02")

  (def data (repeatedly 1000 #(+ (rand) (rand) (rand))))

  (extent data)
  ;; => [0.17965413994792245 2.7460020103629614]

  (def q-scale (scale :quantile {:domain data}))
  q-scale
  ;; => #object[wadogo.common.ScaleType 0x1b72a45c "quantile: [D@68405751 -> (0.25 0.5 0.75 1.0) {:estimation-strategy :r7, :quantiles ([1.148770652600275 0.25] [1.4728260316971906 0.5] [1.8287836732494889 0.75])}"]
  
  (map q-scale (clojure.core/range 0.0 3.0 0.25))
  ;; => (0.25 0.25 0.25 0.25 0.25 0.5 0.75 0.75 1.0 1.0 1.0 1.0)

  (q-scale 1.15 true)
  ;; => {:dstart 1.8287836732494889, :dend 2.934651568076644, :value 1.0, :count 250, :quantile 1.0}
  (inverse q-scale 0.5)
  ;; => {:dstart 1.148770652600275, :dend 1.4728260316971906, :value 0.5, :count 250, :quantile 0.5}

  (def t-scale (scale :threshold {:domain [0.2 10 11] :range [1 2 3 4]}))

  (forward t-scale 0.2)
  )
