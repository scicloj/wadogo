(ns wadogo.scale
  (:refer-clojure :exclude [range])
  (:require [wadogo.common :as common]
            [wadogo.protocols :as proto]
            
            [wadogo.scale.linear]
            [wadogo.scale.interpolated]
            [wadogo.scale.log]
            [wadogo.scale.symlog]
            [wadogo.scale.bands]
            [wadogo.scale.ordinal]
            [wadogo.scale.quantile]))

(defn scale
  ([scale-kind] (common/scale scale-kind))
  ([scale-kind attributes] (common/scale scale-kind attributes)))

(defn forward
  [scale v]
  (proto/forward scale v))

(defn inverse
  [scale v]
  (proto/inverse scale v))

(defn domain
  [scale]
  (proto/domain scale))

(defn range
  [scale]
  (proto/range scale))

(defn data
  ([scale]
   (proto/data scale))
  ([scale key]
   (get (proto/data scale) key)))

(defn kind
  [scale]
  (data scale :kind))

(defn set-domain
  [scale domain]
  (proto/set-domain scale domain))

(defn set-range
  [scale range]
  (proto/set-range scale range))

(defn set-data
  [scale attribute v]
  (proto/set-data scale attribute v))



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

  (set-domain my-scale [200 300])
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x44620b "[200 300] -> [100 200] {:kind :linear}"]
  (set-range my-scale [-10 -20])
  ;; => #object[wadogo.core$reify_scale$reify__25948 0x4519228c "[-1 2] -> [-10 -20] {:kind :linear}"]

  )
