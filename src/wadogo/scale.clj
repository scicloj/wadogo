(ns wadogo.scale
  (:refer-clojure :exclude [range format])
  (:require [java-time :refer [duration]]
            [wadogo.common :as common]

            [wadogo.scale.linear]
            [wadogo.scale.interpolated]
            [wadogo.scale.log]
            [wadogo.scale.symlog]
            [wadogo.scale.pow]
            [wadogo.scale.bands]
            [wadogo.scale.ordinal]
            [wadogo.scale.quantile]
            [wadogo.scale.quantize]
            [wadogo.scale.threshold]
            [wadogo.scale.histogram]
            [wadogo.scale.datetime]
            [wadogo.scale.constant]
            
            [fastmath.stats :as stats])
  (:import [wadogo.common ScaleType]))

(set! *warn-on-reflection* true)

(defn scale
  ([scale-kind] (common/scale scale-kind))
  ([scale-kind attributes] (common/scale scale-kind attributes)))

(defn forward [scale v] (scale v))
(defn inverse [^ScaleType scale v] ((.inverse-fn scale) v))
(defn domain  [^ScaleType scale] (.domain scale))
(defn range [^ScaleType scale] (.range scale))
(defn kind [^ScaleType scale] (.kind scale))

(defn ticks [^ScaleType scale] (common/ticks scale))

(defn format
  ([scale]
   (format scale (ticks scale)))
  ([scale vs]
   (map (common/formatter scale vs) vs)))

(defn formatter
  ([scale]
   (formatter scale (ticks scale)))
  ([scale vs]
   (common/formatter scale vs)))

(defn data
  ([^ScaleType scale]
   (.data scale))
  ([^ScaleType scale key]
   (get (.data scale) key)))

(defn with-domain [^ScaleType scale domain]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :domain domain)) )
(defn with-range [^ScaleType scale range]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :range range)) )
(defn with-kind [^ScaleType scale kind]
  (common/scale kind (assoc (common/scale->map scale) :kind kind)))
(defn with-data
  ([^ScaleType scale data]
   (common/scale (.kind scale) (merge (common/scale->map scale) data)))
  ([^ScaleType scale k v]
   (common/scale (.kind scale) (assoc (common/scale->map scale) k v))))
(defn with-ticks [^ScaleType scale ticks]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :ticks ticks)))
(defn with-formatter [^ScaleType scale fmt]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :formatter fmt)))


(def mapping common/mapping)

(defn- general-size
  [data typ]
  (condp = typ
    :discrete (if (sequential? data) (count data) 1)
    :continuous (Math/abs (- (last data) (first data)))
    :datetime (duration (last data) (first data))
    nil))

(defn size
  ([scale] (size scale :range))
  ([scale range-or-domain]
   (let [[dtype rtype] (-> scale kind mapping)]
     (if (= range-or-domain :domain)
       (general-size (domain scale) dtype)
       (general-size (range scale) rtype)))))

(defn extent [xs]
  (let [[minx maxx] (stats/extent xs)]
    [minx maxx]))

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

  (def q-scale (scale :quantile {:domain data}))
  q-scale
  ;; => #object[wadogo.common.ScaleType 0x1b72a45c "quantile: [D@68405751 -> (0.25 0.5 0.75 1.0) {:estimation-strategy :legacy, :quantiles ([1.148770652600275 0.25] [1.4728260316971906 0.5] [1.8287836732494889 0.75])}"]

  (map q-scale (clojure.core/range 0.0 3.0 0.25))
  ;; => (nil 0.25 0.25 0.25 0.25 0.5 0.75 0.75 1.0 1.0 1.0 1.0)

  (q-scale 1.15 true)
  ;; => {:dstart 1.8287836732494889, :dend 2.934651568076644, :value 1.0, :count 250, :quantile 1.0}
  (inverse q-scale 0.5)
  ;; => {:dstart 1.148770652600275, :dend 1.4728260316971906, :value 0.5, :count 250, :quantile 0.5}
  )
