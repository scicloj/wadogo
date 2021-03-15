# wadogo

Scales for Clojure. Domain -> range transformations of various types. Derived from [`d3-scale`](https://github.com/d3/d3-scale), originally implemented for [`cljplot`](https://github.com/generateme/cljplot)

## Name

`wadogo` supposed to mean `scale` in Swahili according to [google translate](https://translate.google.com/?sl=en&tl=sw&text=scale&op=translate). Quickly appeared that this is not true. But, `wadogo` is cute and means `little`.

## Usage

In progress:

[documentation](https://scicloj.github.io/wadogo/usage/)

## Scale types

### continuous -> continuous

* linear
* logarthmic
* symmetrical log ([read here](https://www.researchgate.net/profile/John_Webber4/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data/links/0fcfd50d791c85082e000000.pdf))
* exponential (pow)
* interpolated
* date/time

### continuous -> discrete

* threshold
* quantize

#### continuous data -> discrete

* quantiles
* histogram

### discrete -> continuous

* bands

### discrete -> discrete

* ordinal

## Ticks and formatting

Every scale generates list of ticks, discrete collection of uniformly distributed nice values which can be used as labels for any scale.

Additionally you can build a formatting function to convert scale values to a string.

### Example session

```clojure
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
(fmt my-scale)
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

(fmt dt-scale)
;; => ("May-06" "May-27" "Jun-17" "Jul-08" "Jul-29" "Aug-19" "Sep-09" "Sep-30" "Oct-21" "Nov-11" "Dec-02")

(def data (repeatedly 1000 #(+ (rand) (rand) (rand))))

(def q-scale (scale :quantile {:domain data}))
q-scale
;; => #object[wadogo.common.ScaleType 0x1b72a45c "quantile: [D@68405751 -> (0.25 0.5 0.75 1.0) {:estimation-strategy :legacy, :quantiles ([1.148770652600275 0.25] [1.4728260316971906 0.5] [1.8287836732494889 0.75])}"]

(map q-scale (clojure.core/range 0.0 3.0 0.25))
;; => (nil 0.25 0.25 0.25 0.25 0.5 0.75 0.75 1.0 1.0 1.0 1.0)

(q-scale 1.15 true)
;; => {:dstart 1.8287836732494889, :dend 2.934651568076644, :value 1.0, :count 250, :quantile 1.0}
(inverse q-scale 0.5)
;; => {:dstart 1.148770652600275, :dend 1.4728260316971906, :value 0.5, :count 250, :quantile 0.5}
```

## License

Copyright © 2021 Scicloj / GenerateMe

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
