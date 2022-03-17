;; # Wadogo scales

;; Wadogo is the library which brings various transformations between domain and range (codomain), either continuous or discrete. The idea is based on [d3-scale](https://github.com/d3/d3-scale) and originally was a part of [cljplot](https://github.com/generateme/cljplot) library.

;; There are 13 different scales with unified api.

;; *The `wadogo` name came up after trying to translate word `scale` into different languages using [google translate](https://translate.google.com/?sl=en&tl=sw&text=scale&op=translate). Swahili word was very pleasant and was choosen on zulip chat by a community. Funny enough is that `wadogo` doesn't mean `scale` at all but `small` or `little`. Translator failed here.* 

^{:nextjournal.clerk/visibility :hide-ns
  :nextjournal.clerk/no-cache true}
(ns index
  (:require [nextjournal.clerk :as clerk]
            [wadogo.scale :as s]
            [fastmath.core :as m]))

^{::clerk/visibility :fold ::clerk/viewer :hide-result}
(do
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
     :layer [{:mark "rule"
              :encoding {:x {:field "rstart"}
                         :x2 {:field "rend"}}}
             {:mark {:type "circle"
                     :stroke "green"
                     :opacity 0.9}
              :encoding {:x {:field "point"}}}]}))

^{::clerk/visibility :hide ::clerk/viewer :hide-result}
(defmacro forms->result-table
  ([rows] `(forms->result-table ["code" "result"] ~rows))
  ([head rows]
   `(clerk/table {:head ~head
                  :rows [~@(map #(vec (conj (seq %) `(quote ~(first %)))) rows)]})))

^{::clerk/visibility :hide ::clerk/viewer :hide-result}
(defmacro ->table
  [& forms]
  `(clerk/table {:head ["results"] :rows [~@(map vector forms)]}))


;; ## Scales

;; The scale is a structure which helps to transform one set of values into another. There are many different ways of mapping between domain and range, collected in the table below.

^{::clerk/visibility #{:hide}}
(clerk/table
 {:head ["scale kind" "domain" "range" "info"]
  :rows [[:linear "continuous, numerical" "continuous, numerical" "linear"]
         [:log "continuous, numerical, positive values" "continuous, numerical" "logarithmic, base=10.0"]
         [:symlog "continuous, numerical" "continuous, numerical" "logarithmic (symmetric), base=10.0"]
         [:pow "continuous, numerical" "continuous, numerical" "power, exponent=0.5"]
         [:interpolated "continuous, numerical, segmented" "continuous, numerical" "interpolated function, linear by default"]
         [:quantize "continuous" "discrete, any or quantization data" "splits domain into evenly sized segments"]
         [:datetime "continuous, temporal" "continuous, numerical" "temporal"]
         [:histogram "data, numerical" "discrete, numerical or bin data" "splits data into bins"]
         [:quantile "data, numerical" "discrete, numerical or quantile data" "splits data into quantiles"]
         [:threshold "continuous, numerical" "discrete, any or segment data" "splits data into segments by given thresholds"]
         [:bands "discrete, any" "discrete, numerical or bin data" "assigns domain values into evenly sized segments"]
         [:ordinal "discrete, any" "discrete, any" "maps domain and range values"]
         [:constant "any" "any" "returns constant value"]]})

;; Scale acts as a mathematical function with defined inverse in most cases. Additionally there is a selection of helper functions to access range, domain, representation values (ticks) and scale modifications.

;; ## Basics

;; Let's import `wadago.scale` name space as an entry point
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
(forms->result-table
 [[(s/kind logarithmic)]
  [(s/domain logarithmic)]
  [(s/range logarithmic)]
  [(s/data logarithmic)]])

;; We can also read particular key from `data` field.

(s/data logarithmic :base)

;; ### Size

;; To get information about size of the domain or range (default), call `size`.

^{::clerk/visibility :hide}
(forms->result-table
 [[(s/size logarithmic :domain)]
  [(s/size logarithmic :range)]
  [(s/size logarithmic)]])

;; ### Updating scale

;; To change some of the fields while keeping the others you can call `with-` functions.

^{::clerk/visibility :hide}
(forms->result-table
 [[(s/with-domain logarithmic [100.0 200.0])]
  [(s/with-range logarithmic [99.0 -99.0])]
  [(s/with-data logarithmic {:anything 11.0})]
  [(s/with-data logarithmic :anything 11.0)]
  [(s/with-data logarithmic :anything 11.0 :something 22)]
  [(s/with-kind logarithmic :linear)]])

;; ### Ticks

;; Every scale is able to produce `ticks`, ie. sequence of values which is taken from domain (or range in certain cases). User can also provide own ticks or expected number of ticks. The exact number of returned ticks can differ from expected number of ticks.

(->table
 (s/ticks logarithmic)
 (s/ticks (s/with-ticks logarithmic 2))
 (s/ticks logarithmic 2) ;; same as above
 (s/ticks (s/with-ticks logarithmic [1 50 500])))

(clerk/vl (cont->cont (s/with-ticks logarithmic [1 50 500])))

;; ### Formatting

;; Ticks (or any values) can be formatter to a string. `wadogo` provides some default formatters for different kind of scales.

;; * `format` - formats ticks or provided sequence
;; * `formatter` returns a formatter

;; Custom formatter can be provided by setting `formatter` key during scale creation.

(->table
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

(->table
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

;; ## Numerical scales (continuous -> continuous)

;; This group of scales transform continuous domain into continuous range of numbers.

;; We have here:

;; * linear
;; * logarithmic
;; * symmetric log
;; * exponential
;; * interpolated

(require '[wadogo.config :as cfg])

;; ### Linear scale

;; Linear scale maps a domain $[d_1, d_2]$ to range $[r_1, r_2]$ using following formula

;; $$scale_{linear}(x) = r_1 + \frac{r_2-r_1}{d_2-d_1}(x-d_1)$$

(def linear-scale (s/scale :linear {:domain [0.0 1.0]
                                  :range [-100.0 100.0]}))

(clerk/vl (cont->cont linear-scale))

(->table
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

(->table
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

(->table
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

(->table
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

;; Flatness parameter $C$ equals $\frac{1}{\ln(base)}$ by default or can be provided by user.

;; $symlog$ function is later normalized to a domain and lineary tranformed to a desired range.

;; $$scale_{symlog}(x) = r_1 + \frac{r_2-r_1}{symlog(d_2)-symlog(d_1)}(symlog(x)-symlog(d_1))$$

(def symmetric-log-scale (s/scale :symlog {:domain [-7.0 5.0]
                                         :range [-20 100]}))

(clerk/vl (cont->cont symmetric-log-scale))

(->table
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

;; Changing base, changes the slope of the curve around `0.0`. We need to reset `:C` as well (if not, previous value will be used).

(def symmetric-log-scale-base-2 (s/with-data symmetric-log-scale :base 2 :C nil))

(clerk/vl (cont->cont symmetric-log-scale-base-2))

;; ### Power scale

;; Power scale uses exponent (`0.5` by default) to scale input.

;; $$scale_{power_a}(x)=r_1+\frac{r_2-r_1}{d_2^a-d_1^a}(x^a-d_1^a)$$

(def power-scale (s/scale :pow {:domain [0 20]
                              :range [-7 3]}))

(clerk/vl (cont->cont power-scale))

(->table
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

;; ### Interpolated scale

;; This scale interpolates between points created from domain and range, ie. pairs $(d_i, r_i)$. `:interpolator` parameter defines the method of interpolation, `:interpolator-params` is a list of parameters for `interpolation` function. Please refer [fastmath.interpolation](https://generateme.github.io/fastmath/fastmath.interpolation.html#var-interpolators-1d-list) namespace for a list of functions and interpolation names.

(def interpolated-scale (s/scale :interpolated {:domain [1 2 4 6 10]
                                              :range [-7 0 2 4 20]}))

(clerk/vl (cont->cont interpolated-scale))

(->table
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

(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :monotone)))
(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :shepard)))
(clerk/vl (cont->cont (s/with-data interpolated-scale :interpolator :loess :interpolator-params [0.7 2])))

;; ## Datetime scale (continuous -> continuous)

;; This is linear scale which domain is a temporal interval and range is numerical. [clojure.java-time](https://github.com/dm3/clojure.java-time) is used as a Java 8 datetime API. Any `java.time.temporal.Temporal` is supported.

(def datetime-scale (s/scale :datetime {:domain [(dt/zoned-date-time 2001 10 20 4)
                                               (dt/zoned-date-time 2010 10 20 5)]
                                      :range [0 100]}))

(->table
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

(->table
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

;; Creates histogram bins from a data. `:range` can be used to define number of bins or a method of estimation (one of: `:sqrt`, `:sturges`, `:rice`, `:doane`, `:scott` or `:freedman-diaconis` same as `:default`).

(defn bin-intervals
  "Create intervals from histogram data"
  [scale]
  (-> (map first (-> scale (s/data :bins)))
      vec (conj (-> scale s/domain s/extent last))
      (->> (partition 2 1))))

(def histogram-scale (s/scale :histogram {:domain data}))

(clerk/vl (cont->discrete histogram-scale bin-intervals))

(->table
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

(->table
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

(->table
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

;; Divides data by quantiles (`:range`). `:estimation-strategy` can be either `:legacy` ([method description](https://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/stat/descriptive/rank/Percentile.html)) or one from `:r1` to `:r9` ([estimation strategies](https://en.wikipedia.org/wiki/Quantile#Estimating_quantiles_from_a_sample))

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

(->table
 (s/range quantile-scale)            ;; calculated range, bins
 (s/domain quantile-scale)           ;; original data, here as java array
 (quantile-scale 0.5)                ;; returns bin id
 (quantile-scale 0.5 true)           ;; returns additional data about bin
 (quantile-scale 0.9 true)          
 (s/inverse quantile-scale 2)        ;; same as above, return bin data
 (s/ticks quantile-scale)            ;; default ticks, same as range
 (s/ticks quantile-scale 2)          ;; 2 ticks
 )          

;; 10 quantiles with R7 strategy

(-> quantile-scale
    (s/with-range [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])
    (s/with-data :estimation-strategy :r7)
    quantile-intervals)

;; ## Bands (discrete -> continuous)

;; Bands scale maps a discrete value from a domain to a numerical value within assigned interval from a range.

;; Intervals (bands) are created by slicing a range and shrinking them with padding parameters. `padding-out` controls outer gaps, `padding-in` controls gaps between bands. `align` controls position of the representative point inside a band. Values should be from `0.0` (no padding, left alignment) to `1.0` (full padding, right alignment).

(def bands-scale (s/scale :bands {:domain (range 5)
                                :range [-5.0 5.0]}))

(clerk/vl
 (bands-chart [(s/with-data bands-scale :name "padding: 0, align: 0.5, default")
               (s/with-data bands-scale :name "padding-out: 0.4" :padding-out 0.4)
               (s/with-data bands-scale :name "padding-in: 0.4" :padding-in 0.4)
               (s/with-data bands-scale :name "padding: 0.8" :padding-out 0.8 :padding-in 0.8)
               (s/with-data bands-scale :name "padding: 0.1" :padding-out 0.1 :padding-in 0.1)
               (s/with-data bands-scale :name "align: 0.2, padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align 0.2)
               (s/with-data bands-scale :name "align: 0.8, padding: 0.1" :padding-out 0.1 :padding-in 0.1 :align 0.8)]))

;; ## Ordinal (discrete -> discrete)

;; Maps values from domain to a range.

(def ordinal-scale (s/scale :ordinal {:domain [:a :b "11" 0]
                                    :range [-3 :some-key [2 3] {:a -11}]}))

(map ordinal-scale [0 :a :b "11" :other-key])

(map (partial s/inverse ordinal-scale) [0 -3 :some-key [2 3] {:a -11}])

;; ## Constant

;; Constant scale just maps domain value into range value (and back for inverse).

(def constant-scale (s/scale :constant {:domain "any domain value"
                                      :range :any-range-value}))

(->table
 (s/range constant-scale)
 (s/domain constant-scale)
 (constant-scale :in)           ;; always returns range
 (s/inverse constant-scale 0)   ;; always returns domain
 )

;; ## Source code

;; [![](https://img.shields.io/clojars/v/org.scicloj/wadogo.svg)](https://clojars.org/org.scicloj/wadogo)

;; Source code is available on [github](https://github.com/scicloj/wadogo).

^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(clerk/hide-result
 (comment
   (clerk/serve! {:browse? false :watch-paths ["."]})
   (clerk/show! "index.clj")
   (clerk/build-static-app! {:paths ["index.clj"] :out-path "docs"})
   (clerk/clear-cache!)
   (clerk/halt!)))
