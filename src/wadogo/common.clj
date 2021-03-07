(ns wadogo.common
  (:refer-clojure :exclude [range])
  (:import [clojure.lang IFn]))

(deftype ScaleType [kind domain range forward-fn inverse-fn data]
  Object
  (toString [_]
    (str (name kind) ": " domain " -> " range (when data (str " " data))))
  IFn
  (invoke [_ v] (forward-fn v)))

(defn scale->map
  [^ScaleType scale]
  (assoc (.data scale)
         :kind (.kind scale)
         :domain (.domain scale)
         :range (.range scale)))

(defmulti scale (fn [k & _] k))
(defmulti ticks (fn [k & _] k))
(defmulti nice (fn [k _] k))


