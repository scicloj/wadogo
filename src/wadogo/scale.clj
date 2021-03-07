(ns wadogo.scale
  (:refer-clojure :exclude [range])
  (:require [wadogo.common :as common]

            [wadogo.scale.linear]
            [wadogo.scale.interpolated]
            [wadogo.scale.log]
            [wadogo.scale.symlog]
            [wadogo.scale.bands]
            [wadogo.scale.ordinal]
            [wadogo.scale.quantile])
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

  )
