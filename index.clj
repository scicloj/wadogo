;; # Wadogo scales

;; Wadogo is the library which brings various transformations between domain and range (codomain), either continuous or discrete. The idea is based on [d3-scale](https://github.com/d3/d3-scale) and originally was a part of [cljplot](https://github.com/generateme/cljplot) library.

;; There are 18 different scales with unified api. `(s/kind-info kind)` returns the full description of a kind.

;; *The `wadogo` name came up after trying to translate word `scale` into different languages using [google translate](https://translate.google.com/?sl=en&tl=sw&text=scale&op=translate). Swahili word was very pleasant and was choosen on zulip chat by a community. Funny enough is that `wadogo` doesn't mean `scale` at all but `small` or `little`. Translator failed here.* 

^{:nextjournal.clerk/visibility :hide-ns
  :nextjournal.clerk/no-cache true
  :nextjournal.clerk/toc true}
(ns index
  (:require [nextjournal.clerk :as clerk]
            [wadogo.scale :as s]
            [fastmath.core :as m]
            [clojure.string :as str]))

^{::clerk/visibility :fold ::clerk/viewer :hide-result}
(defn cont->cont
  ([scale] (cont->cont scale {}))
  ([scale {:keys [w h]
           :or {w 400 h 200}}]
   (let [x1 (first (s/domain scale))
         x2 (last (s/domain scale))]
     {:mode "vega-lite"
      :width w
      :height h
      :data {:values (map (partial zipmap [:domain :range])
                          (map #(vector % (scale %)) (m/slice-range x1 x2 80)))}
      :mark "line" 
      :encoding {:x {:field "domain" :type "quantitative" :axis {:values (s/format scale)}}
                 :y {:field "range" :type "quantitative"}}})))

^{::clerk/visibility :fold ::clerk/viewer :hide-result}
(defn cont->discrete
  [scale selector]
  (let [y (s/range scale)
        x (selector scale)]
    {:mode "vega-lite"
     :width 400 :height 200
     :data {:values (map (fn [x y]
                           {:y y :x1 (first x) :x2 (second x)}) x y)}
     :mark "bar"
     :encoding {:y {:field "y" :type "ordinal"}
                :x {:field "x1" :type "quantitative"}
                :x2 {:field "x2"}}}))

^{::clerk/visibility :fold ::clerk/viewer :hide-result}
(defn bands-chart
  [scales]
  {:mode "vega-lite"
   :width 500 :height 200
   :data {:values (mapcat (fn [scale]
                            (let [n (s/data scale :name)]
                              (map #(assoc % :name n) (s/data scale :bands)))) scales)}
   :encoding {:y {:field "name"
                  :axis {:title nil}}
              :x {:type "quantitative"
                  :axis {:title nil}}}
   :layer [{:data {:values (map (fn [scale]
                                  (let [[r0 r1] (s/range scale)]
                                    {:name (s/data scale :name) :r0 r0 :r1 r1})) scales)}
            :mark {:type "rule" :color "lightgray" :strokeDash [2 2]}
            :encoding {:x {:field "r0"}
                       :x2 {:field "r1"}}}
           {:mark "rule"
            :encoding {:x {:field "rstart"}
                       :x2 {:field "rend"}}}
           {:mark {:type "circle"
                   :stroke "green"
                   :opacity 0.9}
            :encoding {:x {:field "point"}}}]})

^{::clerk/visibility {:code :hide :result :hide}}
(def source-files "https://github.com/scicloj/wadogo/blob/master/src/")

^{::clerk/visibility {:code :hide :result :hide}}
(defn args->call [f args] (conj (seq args) f))
^{::clerk/visibility {:code :hide :result :hide}}
(defn fix-tex [s]  (str/replace s #"\\\\\(|\\\\\)" "\\$"))
^{::clerk/visibility {:code :hide :result :hide}}
(defn fix-anchor [s] (str/replace s #"\[\[(.+?)\]\]" "[$1](#LOS-$1)"))

^{::clerk/visibility {:code :hide :result :hide}}
(defn make-public-fns-table
  [ns]
  (clerk/html
   [:div
    [:div
     [:span [:b {:class "underline decoration-2 decoration-gray-400"} (name ns)] " namespace"]
     [:p]
     [:div (clerk/md (->> (or (:doc (meta (the-ns ns))) "\n") fix-tex fix-anchor))]]
    [:div (for [v (->> (ns-publics ns)
                       (sort-by first)
                       (map second))
                :let [{:keys [name file macro arglists doc line const deprecated]} (meta v)]]
            [:div {:class "pb-8" :id (str "LOS-" (clojure.core/name name))} ;; add border
             
             (clerk/html [:span [:b {:class (str "underline decoration-2 decoration-gray-400"
                                                 (when deprecated " text-gray-400"))} name]
                          (when macro [:sup " MACRO"])
                          (when const [:sup " CONST"])
                          [:sup [:a {:href (str source-files file "#L" line)} "  [source]"]]])
             (when deprecated [:div [:i (if (string? deprecated)
                                          (clerk/md (str "Deprecated: "(->> deprecated fix-tex fix-anchor)))
                                          "Deprecated")]])
             [:p]
             (when const [:div (clerk/code (var-get v)) [:p]])
             (when arglists [:div
                             [:div (for [args arglists]
                                     (clerk/code (args->call name args)))]
                             [:p]])
             [:div (clerk/md (->> (or doc "\n") fix-tex fix-anchor))]])]]))


;; ## Scales

;; The scale is a structure which helps to transform one set of values into another. There are many different ways of mapping between domain and range, collected in the table below.

^{::clerk/visibility #{:hide}}
(clerk/table
 {:head ["scale kind" "domain" "range" "info"]
  :rows [[:linear "continuous, numerical" "continuous, numerical" "linear"]
         [:identity "continuous, numerical" "continuous, numerical, equal to domain" "the value itself"]
         [:log "continuous, numerical, values of one sign" "continuous, numerical" "logarithmic"]
         [:symlog "continuous, numerical" "continuous, numerical" "logarithmic (symmetric), C=1.0"]
         [:pow "continuous, numerical" "continuous, numerical" "power, exponent=0.5"]
         [:radial "continuous, numerical" "continuous, numerical" "radius of an area linear in the domain"]
         [:interpolated "continuous, numerical, segmented" "continuous, numerical" "interpolated function, linear by default"]
         [:sequential "continuous, numerical" "interpolator results" "normalised domain passed to an interpolator (gradient)"]
         [:diverging "continuous, numerical, [low mid high]" "interpolator results" "normalised domain (mid at 0.5) passed to an interpolator"]
         [:quantize "continuous" "discrete, any or quantization data" "splits domain into evenly sized segments"]
         [:datetime "continuous, temporal" "continuous, numerical" "temporal"]
         [:histogram "data, numerical" "discrete, numerical or bin data" "splits data into bins"]
         [:quantile "data, numerical" "discrete, numerical or quantile data" "splits data into quantiles"]
         [:threshold "continuous, numerical" "discrete, any or segment data" "splits data into segments by given thresholds"]
         [:bands "discrete, any" "discrete, numerical or bin data" "assigns domain values into evenly sized segments"]
         [:point "discrete, any" "continuous, numerical" "assigns domain values to evenly spaced points"]
         [:ordinal "discrete, any" "discrete, any" "maps domain and range values"]
         [:constant "any" "any" "returns constant value"]]})

;; Scale acts as a mathematical function with defined inverse in most cases. Additionally there is a selection of helper functions to access range, domain, representation values (ticks) and scale modifications.

;; ### Custom scales

;; Wadogo allows creation of custom continuous numerical scale from provided `forward` and `inverse` functions. See examples below.

;; ## Basics

;; Let's import `wadago.scale` name space as an entry point
#_:clj-kondo/ignore
(require '[wadogo.scale :as s])

;; To illustrate functions we'll use the logarithmic scale mapping `[2.0 5.0]` domain to `[-1.0 1.0]` range. To create any scale we use `scale` multimethod. Parameters are optional and there as some defaults for every scale kind.

(def logarithmic (s/scale :log {:domain [0.5 1001.0]
                              :range [-1.0 1.0]}))

(clerk/vl (cont->cont logarithmic))

;; ### Scaling

;; To perform scaling, use a scale as a function or call `forward` function.

(map logarithmic [0.0 2.0 3.0 6.0 8.0])
(s/forward logarithmic 1.0)

;; In most cases inverse transformation is also possible

(s/inverse logarithmic -1.0)

;; ### Fields

;; Scale itself is a custom type, implementing `IFn` interface and containing following fields:

;; * `kind` - kind of the scale
;; * `domain` - domain of the transformation
;; * `range` - range of the transformation
;; * `data` - any data as a map, some scales store additional information there.

^{::clerk/visibility :hide}
(clerk/example
 (s/kind logarithmic)
 (s/domain logarithmic)
 (s/range logarithmic)
 (s/data logarithmic))

;; We can also read particular key from `data` field.

(s/data logarithmic :base)

;; ### Size

;; To get information about size of the domain or range (default), call `size`.

^{::clerk/visibility :hide}
(clerk/example
 (s/size logarithmic :domain)
 (s/size logarithmic :range)
 (s/size logarithmic))

;; ### Updating scale

;; To change some of the fields while keeping the others you can call `with-` functions.

^{::clerk/visibility :hide}
(clerk/example
 (s/with-domain logarithmic [100.0 200.0])
 (s/with-range logarithmic [99.0 -99.0])
 (s/with-data logarithmic {:anything 11.0})
 (s/with-data logarithmic :anything 11.0)
 (s/with-data logarithmic :anything 11.0 :something 22)
 (s/with-kind logarithmic :linear))

;; ### Ticks

;; Every scale is able to produce `ticks`, ie. sequence of values which is taken from domain (or range in certain cases). User can also provide own ticks or expected number of ticks. The exact number of returned ticks can differ from expected number of ticks.

^{::clerk/visibility :hide}
(clerk/example
 (s/ticks logarithmic)
 (s/ticks (s/with-ticks logarithmic 2))
 (s/ticks logarithmic 2) ;; same as above
 (s/ticks (s/with-ticks logarithmic [1 50 500])))

(clerk/vl (cont->cont (s/with-ticks logarithmic [1 50 500])))

;; #### Pretty ticks

;; Continuous and datetime scales can choose ticks in a different way, set by `:ticks-method`:

;; * `:default` - d3 style ticks (10 by default)
;; * `:pretty` - ticks of R's `pretty()` function (5 by default), only the ones inside the domain. Log scales use subdivisions of powers (multipliers `1..base-1`, for base 10 also 1-2-5 and 1-5) or powers, the grid with number of ticks closest to requested; datetime scales R's `pretty()` for dates.

;; `:ticks-params` can contain `:min-n`, minimal number of intervals (R's `min.n`).

^{::clerk/visibility :hide}
(clerk/example
 (s/ticks (s/scale :linear {:domain [0 150]}) 5)
 (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty}))
 (s/ticks (s/scale :linear {:domain [3 97] :ticks-method :pretty}))
 (s/ticks (s/scale :log {:domain [3 700] :ticks-method :pretty})))

;; #### Nice

;; `nice` extends the domain to the nearest ticks outside, so the domain starts and ends with a tick. The ticks are chosen with the scale's `:ticks` and `:ticks-method`. Discrete scales and scales with a list of ticks are not changed.

^{::clerk/visibility :hide}
(clerk/example
 (s/domain (s/nice (s/scale :linear {:domain [0.13 0.97]})))
 (s/domain (s/nice (s/scale :linear {:domain [3 97] :ticks-method :pretty})))
 (s/ticks (s/nice (s/scale :linear {:domain [3 97] :ticks-method :pretty})))
 (s/domain (s/nice (s/scale :log {:domain [3 7000]})))
 (s/domain (s/nice (s/scale :log {:domain [3 7000] :ticks-method :pretty}))))

;; ### Formatting

;; Ticks (or any values) can be formatter to a string. `wadogo` provides some default formatters for different kind of scales.

;; * `format` - formats ticks or provided sequence
;; * `formatter` returns a formatter

;; Custom formatter can be provided by setting `formatter` key during scale creation.

^{::clerk/visibility :hide}
(clerk/example
 (s/format logarithmic)
 (s/format (s/with-formatter logarithmic (comp str int)))
 ((s/formatter logarithmic) 33.343000001))

;; #### Formatting numbers

;; Formatting ints or doubles can be parametrized by providing `:formatting-params` data map.

;; There are following parameters for doubles

;; * `:digits` - number of decimal digits, digits can be negative or positive, default: `-8`
;;     * positive - decimal part will have exact number of digits (padded by zeros)
;;     * negative - trailing zeros will be truncated
;; * `:threshold` - when to switch into scientific notation, default: `8`
;; * `:na` - how to convert `nil`, default: `"NA"`
;; * `:nan` - how to convert `##NaN`, default: `"NaN"`
;; * `:inf` - how to convert `##Inf`, default: `"∞"`
;; * `:-inf` - how to convert `##-Inf`, default: `"-∞"`

(s/format (s/scale :linear {:formatter-params {:digits 4}}))

;; In case of integers, parameters are:

;; * `:digits` - number of digits with padding with leading zeros, default: `0` (no leading zeros)
;; * `:hex?` - print as hexadecimal number
;; * `:na` - how to convert `nil`, default: `"NA"`

(s/format (s/scale :quantize {:range [0 2 4 6 9 nil 11111]
                              :formatter-params {:hex? true
                                                 :digits 4}}))

;; #### Datetime

;; In case of `datetime` scale, default formatter recognizes datetime domain and adopt format accordingly. You may use `java-time/format` to create own formtatter.

(require '[java-time :as dt])

(def years-scale (s/scale :datetime {:domain [(dt/local-date 2012)
                                            (dt/local-date 2101)]}))

(def minutes-scale (s/scale :datetime {:domain [(dt/local-time 12 1 0 0)
                                              (dt/local-time 12 15 0 0)]}))
^{::clerk/visibility :hide}
(clerk/example
 (s/ticks years-scale) ;; ticks
 (s/format years-scale) ;; formatted ticks
 (s/ticks minutes-scale)
 (s/format minutes-scale))

;; ### Other

;; To convert any scale to a map, call `->map` function.

(s/->map logarithmic)

;; List of all scales and type of their transformation is stored in `scale-kinds` and `mapping` vars.

s/scale-kinds
s/mapping

;; To verify if given object is scale, use `scale?` predicate

(s/scale? (s/scale :linear))
(s/scale? nil)

;; ### Clamp, unknown, round

;; Scales with a continuous range accept three more attributes (as in d3): `:clamp` constrains input values to the domain (and inverse results too), `:unknown` is returned for `nil` and `NaN`, `:round` rounds results half up.

(def unit-scale (s/scale :linear {:domain [0 10] :range [0 100]}))

(clerk/example
 (unit-scale 12.34)                                         ;; extrapolated
 ((s/with-data unit-scale :clamp true) 12.34)               ;; clamped
 (s/inverse (s/with-data unit-scale :clamp true) 150)       ;; inverse is clamped too
 ((s/with-data unit-scale :round true) 1.234)               ;; rounded
 (unit-scale nil)                                           ;; unknown, nil by default
 ((s/with-data unit-scale :unknown -1) ##NaN))              ;; unknown value

;; ## Numerical scales (continuous -> continuous)

;; This group of scales transform continuous domain into continuous range of numbers.

;; We have here:

;; * linear
;; * identity
;; * logarithmic
;; * symmetric log
;; * power
;; * radial
;; * interpolated
;; * sequential and diverging

(require '[wadogo.config :as cfg])

;; ### Linear scale

;; Linear scale maps a domain $[d_1, d_2]$ to range $[r_1, r_2]$ using following formula

;; $$scale_{linear}(x) = r_1 + \frac{r_2-r_1}{d_2-d_1}(x-d_1)$$

(def linear-scale (s/scale :linear {:domain [0.0 1.0]
                                  :range [-100.0 100.0]}))

(clerk/vl (cont->cont linear-scale))

(clerk/example
 (cfg/default-params :linear)                     ;; default domain and range
 (s/scale :linear)                                ;; default scale, identity
 (linear-scale 0.2)                               ;; forward scaling
 ((s/with-range linear-scale [100.0 -100.0]) 0.2) ;; reversed range
 (s/inverse linear-scale 0.2)                     ;; inverse scaling
 (s/ticks linear-scale)                           ;; default ticks
 (s/ticks linear-scale 3)                         ;; 3 ticks
 (s/format linear-scale)                          ;; ticks after formatting
 )


;; Scale from data also can be created:

(def linear-scale-data-domain
  (s/with-domain linear-scale (repeatedly 10 rand)))

(clerk/vl (cont->cont linear-scale-data-domain))

(clerk/example
 (s/domain linear-scale-data-domain)       ;; domain from data
 (linear-scale-data-domain 0.2)            ;; forward scaling
 (s/inverse linear-scale-data-domain 0.2)  ;; inverse scaling
 (s/ticks linear-scale-data-domain)        ;; default ticks
 (s/format linear-scale-data-domain)       ;; ticks after formatting
 )

;; ### Logarithmic scale

;; Logarithmic scale transforms lineary log of value from log of domain $[d_1,d_2]$ into range $[r_1,r_2]$. Domain shouldn't include `0.0`.

;; $$\begin{align}scale_{log}(x) & = r_1 + \frac{r_2-r_1}{\log(d_2)-\log(d_1)}(\log(x)-\log(d_1)) \\ 
;;   & =r_1 + (r_2-r_1)\log_{\frac{d2}{d1}}(\frac{x}{d_1})\end{align}$$

(def log-scale (s/scale :log {:domain [0.1 100]
                            :range [-100 100]}))

(clerk/vl (cont->cont log-scale))

(clerk/example
 (cfg/default-params :log)                     ;; default domain and range
 (s/scale :log)                                ;; default scale
 (log-scale 0.2)                               ;; forward scaling
 ((s/with-range log-scale [100.0 -100.0]) 0.2) ;; reversed range
 (s/inverse log-scale 0.2)                     ;; inverse scaling
 (s/ticks log-scale)                           ;; default ticks
 (s/ticks log-scale 3)                         ;; 3 ticks
 (s/format log-scale)                          ;; ticks after formatting
 )

;; Let's construct scale with base `2`, negative domain and reversed range.
;; Scaling doesn't rely on base, the result for `base=2` is the same as for `base=10`. The only difference is in ticks.

(def log-scale-base-2
  (-> log-scale
      (s/with-data :base 2.0)
      (s/with-domain [-0.25 -256])
      (s/with-range [100 10])))

(clerk/vl (cont->cont log-scale-base-2))

(clerk/example
 (log-scale-base-2 -16)            ;; forward scaling
 (s/inverse log-scale-base-2 50 )  ;; inverse scaling
 (s/ticks log-scale-base-2)        ;; default ticks
 (s/format log-scale-base-2)       ;; ticks after formatting
 (s/format (-> log-scale
               (s/with-domain [1 5000])
               (s/with-data :base 16))) ;; ticks with another base and domain
 )

;; ### Symmetric log scale

;; In case where domain includes `0.0`, symmetric log scale can be used.

;; $$symlog_{C,b}(x)=sgn(x)\log_b\left( 1+\left| \frac{x}{C}\right| \right)$$

;; Flatness parameter $C$ equals $1$ by default (as in d3). The base of the logarithm cancels after normalisation, so `:base` is deprecated and ignored.

;; $symlog$ function is later normalized to a domain and lineary tranformed to a desired range.

;; $$scale_{symlog}(x) = r_1 + \frac{r_2-r_1}{symlog(d_2)-symlog(d_1)}(symlog(x)-symlog(d_1))$$

(def symmetric-log-scale (s/scale :symlog {:domain [-7.0 5.0]
                                         :range [-20 100]}))

(clerk/vl (cont->cont symmetric-log-scale))

(clerk/example
 (cfg/default-params :symlog)                     ;; default domain and range
 (s/scale :symlog)                                ;; default scale
 (symmetric-log-scale 0.2)                               ;; forward scaling
 ((s/with-range symmetric-log-scale [100.0 -100.0]) 0.2) ;; reversed range
 (s/inverse symmetric-log-scale 0.2)                     ;; inverse scaling
 (s/ticks symmetric-log-scale)                           ;; default ticks
 (s/ticks symmetric-log-scale 3)                         ;; 3 ticks
 (s/format symmetric-log-scale)                          ;; ticks after formatting
 )

;; Ticks are the same as in linear scale.

;; Changing `:C` changes the slope of the curve around `0.0`: values much smaller than `C` are mapped almost linearly.

(def symmetric-log-scale-c (s/with-data symmetric-log-scale :C 0.1))

(clerk/vl (cont->cont symmetric-log-scale-c))

;; ### Power scale

;; Power scale uses exponent (`0.5` by default) to scale input.

;; $$scale_{power_a}(x)=r_1+\frac{r_2-r_1}{d_2^a-d_1^a}(x^a-d_1^a)$$

(def power-scale (s/scale :pow {:domain [0 20]
                              :range [-7 3]}))

(clerk/vl (cont->cont power-scale))

(clerk/example
 (cfg/default-params :pow)                     ;; default domain and range
 (s/scale :pow)                                ;; default scale
 (power-scale 0.2)                               ;; forward scaling
 ((s/with-range power-scale [100.0 -100.0]) 0.2) ;; reversed range
 (s/inverse power-scale 0.2)                     ;; inverse scaling
 (s/ticks power-scale)                           ;; default ticks
 (s/ticks power-scale 3)                         ;; 3 ticks
 (s/format power-scale)                          ;; ticks after formatting
 )

;; Other exponenents can be set by changing `:exponent` data.

(def power-2-scale (s/with-data power-scale :exponent 2.0))

(clerk/vl (cont->cont power-2-scale))

;; ### Identity scale

;; Returns the value itself, the range is the domain. Ticks and `nice` work as for the linear scale.

(def identity-scale (s/scale :identity {:domain [0.3 9.7]}))

(clerk/example
 (identity-scale 2.5)
 (s/ticks identity-scale)
 (s/range (s/nice identity-scale)))

;; ### Radial scale

;; Maps the domain linearly to squared range and returns the square root, so the range values are radii of circles with areas proportional to the domain values.

(def radial-scale (s/scale :radial {:domain [0 100] :range [0 10]}))

(clerk/vl (cont->cont radial-scale))

(clerk/example
 (radial-scale 25)
 (s/inverse radial-scale 5))

;; ### Interpolated scale

;; This scale interpolates between points created from domain and range, ie. pairs $(d_i, r_i)$. `:interpolator` parameter defines the method of interpolation, `:interpolator-params` is a list of parameters for `interpolation` function. Please refer [fastmath.interpolation](https://generateme.github.io/fastmath/fastmath.interpolation.html#var-interpolators-1d-list) namespace for a list of functions and interpolation names.

(def interpolated-scale (s/scale :interpolated {:domain [1 2 4 6 10]
                                              :range [-7 0 2 4 20]}))

(clerk/vl (cont->cont interpolated-scale))

(clerk/example
 (cfg/default-params :interpolated)                     ;; default domain and range
 (s/scale :interpolated)                                ;; default scale
 (interpolated-scale 0.2)                               ;; forward scaling
 ((s/with-range interpolated-scale [100.0 -100.0]) 0.2) ;; reversed range
 (s/inverse interpolated-scale 0.2)                     ;; inverse scaling
 (s/ticks interpolated-scale)                           ;; default ticks
 (s/ticks interpolated-scale 3)                         ;; 3 ticks
 (s/format interpolated-scale)                          ;; ticks after formatting
 )

;; ### Other interpolators

;; A scale should be monotone, so use monotone data and a method which preserves monotonicity. `:pchip` and `:monotone` (Fritsch-Carlson) are smooth and monotone:

(def pchip-scale (s/with-data interpolated-scale :interpolator :pchip))

(clerk/vl (cont->cont pchip-scale))

(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :monotone)))

;; Step methods give a piecewise constant scale (`:step` changes value in the middle between points, `:step-before` and `:step-after` at the points):

(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :step)))

(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :step-after)))

;; The inverse solves `forward(x) = y` by bisection, so it's exact for any monotone method. Step methods return the start of a plateau, or the position of a jump for values inside it:

(clerk/example
 (s/inverse interpolated-scale (interpolated-scale 3))  ;; linear
 (s/inverse pchip-scale (pchip-scale 3))                ;; pchip
 (s/inverse (s/with-data interpolated-scale :interpolator :step-after) 1)) ;; jump at 4

;; Methods like `:cubic`, `:loess` or `:shepard` can overshoot between points (`:cubic` here decreases between about `3.2` and `4.2`), so they are not suitable for scales.

;; ### Sequential and diverging scales

;; Sequential scale normalises the domain to $[0,1]$ and passes the result to an `:interpolator` function (identity by default), usually a color gradient. Diverging scale does the same with a three-value domain `[low mid high]` mapped to $0$, $0.5$ and $1$. `:transform` changes the normalisation: `:linear` (default), `:log`, `:pow` (with `:exponent`) or `:symlog` (with `:C`).

(def sequential-scale (s/scale :sequential {:domain [1 1000] :transform :log}))

(clerk/vl (cont->cont sequential-scale))

(def diverging-scale (s/scale :diverging {:domain [-3 0 9]}))

(clerk/vl (cont->cont diverging-scale))

(clerk/example
 (sequential-scale 10)
 (s/ticks sequential-scale)
 (diverging-scale 4.5)
 (s/inverse diverging-scale 0.25)
 ((s/scale :sequential {:domain [0 10] :interpolator (fn [t] (str "color at " t))}) 5))

;; ## Datetime scale (continuous -> continuous)

;; This is linear scale which domain is a temporal interval and range is numerical. [clojure.java-time](https://github.com/dm3/clojure.java-time) is used as a Java 8 datetime API. Supported types: `LocalDateTime`, `LocalDate`, `LocalTime`, `java.util.Date`, and `Instant`, `ZonedDateTime`, `OffsetDateTime`, which are converted to UTC `LocalDateTime`.

(def datetime-scale (s/scale :datetime {:domain [(dt/zoned-date-time 2001 10 20 4)
                                               (dt/zoned-date-time 2010 10 20 5)]
                                      :range [0 100]}))

(clerk/example
 (cfg/default-params :datetime)                     ;; default domain and range
 (s/scale :datetime)                                ;; default scale
 (datetime-scale (dt/zoned-date-time 2002 10 20 4)) ;; forward scaling
 (s/inverse datetime-scale 0.2)                     ;; inverse scaling
 (s/ticks datetime-scale)                           ;; default ticks
 (s/ticks datetime-scale 3)                         ;; 3 ticks
 (s/format datetime-scale)                          ;; ticks after formatting
 )

(def time-scale (s/with-domain datetime-scale [(dt/local-time 12)
                                             (dt/local-time 13)]))

(clerk/example
 (time-scale (dt/local-time 12 13)) ;; forward scaling
 (s/inverse time-scale 0.2)         ;; inverse scaling
 (s/ticks time-scale)               ;; default ticks
 (s/ticks time-scale 3)             ;; 3 ticks
 (s/format time-scale)              ;; ticks after formatting
 )

;; ## Slicing data (continuous -> discrete)

;; This set of scales creates discrete points from slicing data or interval (as a domain) into smaller intervals. Following scales are defined:

;; * histogram
;; * quantile
;; * quantize
;; * threshold

;; Forward scaling by default returns interval id. If second (optional) parameter is set to true, a map with interval details is returned. Inverse scaling accepts interval id and also returns a map with interval info.
                                        ;
;; Additionally `s/data` contains some internal interval information.

;; Intervals are constructed following way. First is $(-\infin,v_1)$, second is $[v_1, v_2)$, ..., and last is $[v_n,\infin)$ 

(def data (map m/sq (repeatedly 150 rand))) 

;; ### Histogram

;; Creates histogram bins from a data. `:range` can be used to define number of bins or a method of estimation (one of: `:sqrt`, `:sturges`, `:rice`, `:terrell-scott`, `:doane`, `:scott` or `:freedman-diaconis` (default, `:default` is an alias)).

(defn bin-intervals
  "Create intervals from histogram data"
  [scale]
  (-> (map first (-> scale (s/data :bins)))
      vec (conj (-> scale s/domain s/extent last))
      (->> (partition 2 1))))

(def histogram-scale (s/scale :histogram {:domain data}))

(clerk/vl (cont->discrete histogram-scale bin-intervals))

(clerk/example
 (s/range histogram-scale)            ;; calculated range, bins
 (s/domain histogram-scale)           ;; original data, here as java array
 (histogram-scale 0.5)                ;; returns bin id
 (histogram-scale 0.5 true)           ;; returns additional data about bin
 (s/inverse histogram-scale 2)        ;; same as above, returns bin data
 (s/ticks histogram-scale)            ;; default ticks, same as range
 (s/ticks histogram-scale 3)          ;; 3 ticks
 (s/data histogram-scale)             ;; bins data [start point,  bin count]
 )          

;; Different bin sizes

(bin-intervals (s/with-range histogram-scale :sturges))
(bin-intervals (s/with-range histogram-scale 12))

;; ### Quantized data

;; It's actually the same as `:histogram` with the difference in `range` which is a list of bin names. 

(defn quantize-intervals
  [scale]
  (let [[start end] (s/extent (s/domain scale))]
    (-> scale (s/data :thresholds)
        (conj start) vec (conj end)
        (->> (partition 2 1)))))

(def quantize-scale (s/scale :quantize {:domain data
                                      :range [:a :b :c "fourth" 5 ::last]}))

(clerk/vl (cont->discrete quantize-scale quantize-intervals))

(clerk/example
 (s/range quantize-scale)            ;; range, bin names
 (s/domain quantize-scale)           ;; interval (from data)
 (quantize-scale 0.5)                ;; returns bin name
 (quantize-scale 0.5 true)           ;; returns additional data about bin
 (s/inverse quantize-scale :b)       ;; same as above, returns bin data
 (s/ticks quantize-scale)            ;; default ticks, same as range
 (s/ticks quantize-scale 3)          ;; 3 ticks
 (s/data quantize-scale)             ;; interval start points
 )          

;; ### Thresholds

;; Domain should contain values which define interval endpoints. Each interval is assigned to a given range value.

(def threshold-scale (s/scale :threshold {:domain [0 1 4 5 9]
                                        :range [:a :b :c "fourth" 5 ::last]}))

(clerk/example
 (s/range threshold-scale)            ;; range, interval names
 (s/domain threshold-scale)           ;; interval start/end points
 (threshold-scale 4.5)                ;; returns interval name
 (threshold-scale 4.5 true)           ;; returns additional data about interval
 (s/inverse threshold-scale :b)       ;; same as above, returns bin data
 (s/ticks threshold-scale)            ;; default ticks, same as range
 (s/ticks threshold-scale 3)          ;; 3 ticks
 (s/data threshold-scale)             ;; interval start points
 )          

;; ### Quantiles

;; Divides data by quantiles (`:range`). `:estimation-strategy` is one from `:r1` to `:r9` ([estimation strategies](https://en.wikipedia.org/wiki/Quantile#Estimating_quantiles_from_a_sample)), `:r7` by default (as d3 and R), or `:legacy` ([method description](https://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/stat/descriptive/rank/Percentile.html)). Values outside the data belong to the first or the last interval.

(defn quantile-intervals
  "Create intervals from quantile scale data."
  [scale]
  (let [[start end] (s/extent (s/domain scale))]
    (-> (map first (-> scale
                       (s/data :quantiles)))
        (conj start) vec (conj end)
        (->> (partition 2 1)))))

(def quantile-scale (s/scale :quantile {:domain data}))

(clerk/vl (cont->discrete quantile-scale quantile-intervals))

(clerk/example
 (s/range quantile-scale)            ;; calculated range, bins
 (s/domain quantile-scale)           ;; original data, here as java array
 (quantile-scale 0.5)                ;; returns bin id
 (quantile-scale 0.5 true)           ;; returns additional data about bin
 (quantile-scale 0.9 true)          
 (s/inverse quantile-scale 2)        ;; same as above, return bin data
 (s/ticks quantile-scale)            ;; default ticks, same as range
 (s/ticks quantile-scale 2)          ;; 2 ticks
 )          

;; 10 quantiles with the legacy strategy

(-> quantile-scale
    (s/with-range [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])
    (s/with-data :estimation-strategy :legacy)
    quantile-intervals)

;; ## Bands (discrete -> continuous)

;; Bands scale maps a discrete value from a domain to a numerical value within assigned interval from a range.

;; Intervals (bands) are created by slicing a range and shrinking them with padding parameters. `padding-out` controls outer gaps, `padding-in` controls gaps between bands. `align` controls position of the representative point inside a band. Values should be from `0.0` (no padding, left alignment) to `1.0` (full padding, right alignment). `align-outer` distributes the outer gaps (d3 `align`, `0.5` by default), `round` rounds the layout to whole numbers. `inverse` returns the domain value of the band containing a number.

;; Align can be:

;; * a number (from `0.0` to `1.0`)
;; * `:spread` which spreads a point from left to right for each band consecutively.
;; * a list of numbers

(def bands-scale (s/scale :bands {:domain (range 5)
                                :range [-5.0 5.0]}))

(clerk/vl
 (bands-chart [(s/with-data bands-scale :name "padding: 0, align: 0.5, default")
               (s/with-data bands-scale :name "padding-out: 0.4" :padding-out 0.4)
               (s/with-data bands-scale :name "padding-in: 0.4" :padding-in 0.4)
               (s/with-data bands-scale :name "padding: 0.8" :padding-out 0.8 :padding-in 0.8)
               (s/with-data bands-scale :name "padding: 0.1" :padding-out 0.1 :padding-in 0.1)
               (s/with-data bands-scale :name "align: 0.2, padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align 0.2)
               (s/with-data bands-scale :name "align: 0.8, padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align 0.8)
               (s/with-data bands-scale :name "align: :spread, padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align :spread)
               (s/with-data bands-scale :name "align: [list], padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align [0.2 0.8 0.5 0.1 0.9])]))

;; ## Point (discrete -> continuous)

;; Point scale is a bands scale with zero-width bands: domain values are mapped to evenly spaced points. `padding` sets the outer gaps as a proportion of the step, `align-outer` distributes them.

(def point-scale (s/scale :point {:domain [:a :b :c :d] :range [0 100] :padding 0.5}))

;; Points (circles) inside the range (dashed line):

(clerk/vl
 (bands-chart [(s/with-data point-scale :name "padding: 0.5, default")
               (s/with-data point-scale :name "padding: 0" :padding 0)
               (s/with-data point-scale :name "padding: 2" :padding 2)
               (s/with-data point-scale :name "padding: 2, align-outer: 0" :padding 2 :align-outer 0)
               (s/with-data point-scale :name "padding: 2, align-outer: 1" :padding 2 :align-outer 1)
               (s/with-data point-scale :name "single value" :domain [:a])]))

(clerk/example
 (map point-scale [:a :b :c :d])
 (s/data point-scale :step)
 (s/inverse point-scale 40))

;; ## Ordinal (discrete -> discrete)

;; Maps values from domain to a range. A shorter range is cycled. When types are mixed, don't forget to turn off sorting (an exception is thrown otherwise). By default domain will be sorted.

(def ordinal-scale (s/scale :ordinal {:domain [:a :b "11" 0]
                                    :range [-3 :some-key [2 3] {:a -11}]
                                    :sort? false}))

(map ordinal-scale [0 :a :b "11" :other-key])

(map (s/scale :ordinal {:domain [:a :b :c :d :e] :range [:odd :even]}) [:a :b :c :d :e])

(map (partial s/inverse ordinal-scale) [0 -3 :some-key [2 3] {:a -11}])

;; ## Constant

;; Constant scale just maps domain value into range value (and back for inverse).

(def constant-scale (s/scale :constant {:domain "any domain value"
                                      :range :any-range-value}))
^{::clerk/visibility :hide}
(clerk/example
 (s/range constant-scale)
 (s/domain constant-scale)
 (constant-scale :in)           ;; always returns range
 (s/inverse constant-scale 0)   ;; always returns domain
 )

;; ## Custom scales

;; Creation of custom continuous numerical scale can be done with `defcustom` function. `forward` and `inverse` functions should be provided as parameters (along with a scale name).

;; Let's create some scales based on [ggplot2 transformations](https://ggplot2-book.org/scale-position.html#scale-transformation) and [Sigmoid wiki](https://en.wikipedia.org/wiki/Sigmoid_function)

;; ### asn

(s/defcustom :asn #(m/atanh %) #(m/tanh %))

(def asn-scale (s/scale :asn {:domain [-0.99 0.99]
                            :range [0.0 1.0]}))

(clerk/vl (cont->cont asn-scale))

^{::clerk/visibility :hide}
(clerk/example
 (asn-scale 0.2)
 (s/inverse asn-scale 0.5383))

;; ### logit

(s/defcustom :logit m/logit m/sigmoid)

(def logit-scale (s/scale :logit {:domain [0.1 0.9]
                                :range [0.0 1.0]}))

(clerk/vl (cont->cont logit-scale))

^{::clerk/visibility :hide}
(clerk/example
 (logit-scale 0.2)
 (s/inverse logit-scale 0.1845))

;; ### probit

(require '[fastmath.random :as rnd])

(s/defcustom :probit #(rnd/cdf rnd/default-normal %) #(rnd/icdf rnd/default-normal %))

(def probit-scale (s/scale :probit {:domain [-5.0 5.0]
                                  :range [0.0 1.0]}))

(clerk/vl (cont->cont probit-scale))

^{::clerk/visibility :hide}
(clerk/example
 (probit-scale 0.2)
 (s/inverse probit-scale 0.5793))

;; ### reciprocal

(s/defcustom :reciprocal #(/ %) #(/ %))

(def reciprocal-scale (s/scale :reciprocal {:domain [0.1 5.0]
                                          :range [0.0 1.0]}))

(clerk/vl (cont->cont reciprocal-scale))

^{::clerk/visibility :hide}
(clerk/example
 (reciprocal-scale 0.2)
 (s/inverse reciprocal-scale 0.510204))

;; ### Gudermannian

(s/defcustom :gudermannian
  #(* 2.0 (m/atan (m/tanh (* 0.5 %))))
  #(* 2.0 (m/atanh (m/tan (* 0.5 %)))))

(def gudermannian-scale (s/scale :gudermannian {:domain [-2.0 2.0]
                                              :range [0.0 1.0]}))

(clerk/vl (cont->cont gudermannian-scale))

^{::clerk/visibility :hide}
(clerk/example
 (gudermannian-scale 0.2)
 (s/inverse gudermannian-scale 0.5763))

;; ### Algebraic

(s/defcustom :algebraic
  #(/ % (m/sqrt (inc (* % %))))
  #(/ % (m/sqrt (- 1.0 (* % %)))))

(def algebraic-scale (s/scale :algebraic {:domain [-2.0 2.0]
                                        :range [0.0 1.0]}))

(clerk/vl (cont->cont algebraic-scale))

^{::clerk/visibility :hide}
(clerk/example
 (algebraic-scale 0.2)
 (s/inverse algebraic-scale 0.6096))

;; ### Distribution

;; You can also make scale from any probability distribution based on `cdf` and `icdf` functions.

#_:clj-kondo/ignore
(require '[fastmath.random :as rng])

(def pascal-distribution (rng/distribution :pascal {:p 0.3 :r 10}))

(s/defcustom :pascal
  (partial rng/cdf pascal-distribution)
  (partial rng/icdf pascal-distribution))

(def pascal-scale (s/scale :pascal {:domain [0.0 20.0]
                                  :range [0.0 1.0]}))

(clerk/vl (cont->cont pascal-scale))

^{::clerk/visibility :hide}
(clerk/example
 (pascal-scale 10.0)
 (s/inverse pascal-scale 0.1166))


;; ## Source code

;; [![](https://img.shields.io/clojars/v/org.scicloj/wadogo.svg)](https://clojars.org/org.scicloj/wadogo)

;; Source code is available on [github](https://github.com/scicloj/wadogo).

;; # List of functions

^{::clerk/visibility :hide}
(make-public-fns-table 'wadogo.scale)

;; # Scale kinds

;; Descriptions returned by `kind-info`: attributes with defaults, mapping, ticks and corner cases.

^{::clerk/visibility {:code :fold :result :hide}}
(defn make-kinds-table
  []
  (clerk/html
   [:div (for [k s/scale-kinds]
           [:div {:class "pb-8" :id (str "KIND-" (name k))}
            [:span [:b {:class "underline decoration-2 decoration-gray-400"} (str k)]]
            [:p]
            [:div (clerk/md (->> (s/kind-info k) fix-tex fix-anchor))]])]))

^{::clerk/visibility :hide}
(make-kinds-table)

^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(comment
  (clerk/serve! {:browse? false})
  (clerk/show! "index.clj")
  (clerk/build! {:paths ["index.clj"] :out-path "docs"})
  (clerk/clear-cache!)
  (clerk/recompute!)
  (clerk/halt!))

