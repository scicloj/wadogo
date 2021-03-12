(ns wadogo.scale
  (:refer-clojure :exclude [range])
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
            [wadogo.scale.constant])
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

(defn fmt [^ScaleType scale]
  (let [tcks (ticks scale)]
    (map (common/fmt scale tcks) tcks)))

(defn data
  ([^ScaleType scale]
   (.data scale))
  ([^ScaleType scale key]
   (get (.data scale) key)))

(defn with-domain [^ScaleType scale domain]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :domain domain)) )
(defn with-range [^ScaleType scale range]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :range range)) )
(defn with-data
  ([^ScaleType scale data]
   (common/scale (.kind scale) (merge (common/scale->map scale) data)))
  ([^ScaleType scale k v]
   (common/scale (.kind scale) (assoc (common/scale->map scale) k v))))
(defn with-ticks [^ScaleType scale ticks]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :ticks ticks)))
(defn with-fmt [^ScaleType scale fmt]
  (common/scale (.kind scale) (assoc (common/scale->map scale) :fmt fmt)))

(def mapping common/mapping)

(defn- general-size
  [data typ]
  (condp = typ
    :discrete (if (sequential? data) (count data) 1)
    :continuous (- (last data) (first data))
    :datetime (duration (last data) (first data))
    nil))

(defn size
  ([scale] (size scale :range))
  ([scale range-or-domain]
   (let [[dtype rtype] (-> scale kind mapping)]
     (if (= range-or-domain :domain)
       (general-size (domain scale) dtype)
       (general-size (range scale) rtype)))))



(comment




  (scale :linear)
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x2c082350 "[0.0 1.0] -> [0.0 1.0] {:kind :linear}"]
  (scale :linear {:domain [10 20]})
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x13bfe19d "[10 20] -> [0.0 1.0] {:kind :linear}"]
  (scale :linear {:domain [-1 2]
                  :range [100 200]})
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x46a2e69f "[-1 2] -> [100 200] {:kind :linear}"]

  (def my-scale (scale :linear {:domain [-1 2]
                                :range [100 200]}))
  ;; => #'wadogo.core/my-scale

  (my-scale 0) ;; => 133.33333333333331
  (forward my-scale 0) ;; => 133.33333333333331
  (inverse my-scale 150) ;; => 0.5

  (domain my-scale) ;; => [-1 2]
  (range my-scale) ;; => [100 200]
  (kind my-scale)

  (with-domain my-scale [200 300])
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x44620b "[200 300] -> [100 200] {:kind :linear}"]
  (with-range my-scale [-10 -20])
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x4519228c "[-1 2] -> [-10 -20] {:kind :linear}"]

  

  (def dt1 (java.time.LocalDateTime/of 2012 5 5 10 10 10))
  (def dt2 (java.time.LocalDateTime/of 2012 12 5 10 10 10))
  (def dt-scale (scale :datetime {:domain [dt1 dt2]}))

  (require '[wadogo.ticks.datetime :as tdt]
           '[wadogo.format.datetime :as fdt])


  (fmt dt-scale)
  
  (def dt-ticks (tdt/datetime-ticks dt1 dt2 (data dt-scale :millis) 10))

  ((fdt/time-format dt-ticks) (first dt-ticks)))


