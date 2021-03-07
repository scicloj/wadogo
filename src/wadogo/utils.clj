(ns wadogo.utils
  (:require [fastmath.interpolation :as i]
            [fastmath.core :as m]))

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

(defn make-norm
  "If domain or range has degenerated interval, treat it special"
  [^double dstart ^double dend ^double rstart ^double rend]
  (if (== dstart dend)
    (let [half (m/mlerp rstart rend 0.5)]
      (fn [^double v]
        (cond
          (< v dstart) rstart
          (> v dstart) rend
          :else half)))
    (m/make-norm dstart dend rstart rend)))

(defn strip-keys [m] (dissoc m :domain :range :kind))
