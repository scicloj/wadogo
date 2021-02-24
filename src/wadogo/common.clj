(ns wadogo.common
  (:refer-clojure :exclude [range])
  (:require [fastmath.interpolation :as i]
            [wadogo.protocols :as proto])
  (:import [clojure.lang IFn]))

;; utils

(defn deep-merge
  "Merge maps on every level."
  [a b] (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn ensure-seq-content
  "If `a` is empty and we have something in `b` - create seq of consecutive numbers."
  [a b]
  (vec (if (and (not a) b)
         (clojure.core/range (count b))
         a)))

(defn interval-steps-before
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp unchecked-int (i/step-before steps (clojure.core/range (count steps)))))

;;

(defmulti scale (fn [k & _] k))
(defmulti ticks (fn [k & _] k))
(defmulti nice (fn [k _] k))

(defn reify-scale
  [forward-fn inverse-fn params]
  (let [scale-kind (:kind params)]
    (reify
      Object
      (toString [_]
        (str (:domain params) " -> " (:range params) " " (dissoc params :domain :range)))
      IFn
      (invoke [_ v] (forward-fn v))
      proto/ScaleProto
      (forward [_ v] (forward-fn v))
      (inverse [_ v] (inverse-fn v))
      proto/ScaleGettersProto
      (domain [_] (:domain params))
      (range [_] (:range params))
      (data [_] params)
      proto/ScaleSettersProto
      (set-domain [_ d] (scale scale-kind (assoc params :domain d)))
      (set-range [_ r] (scale scale-kind (assoc params :range r)))
      (set-data [_ param-name v] (scale scale-kind (assoc params param-name v))))))
