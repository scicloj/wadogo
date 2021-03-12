(ns wadogo.common
  (:refer-clojure :exclude [range])
  (:import [clojure.lang IFn]))

(deftype ScaleType [kind domain range forward-fn inverse-fn data]
  Object
  (toString [_]
    (str (name kind) ": " domain " -> " range (when data (str " " data))))
  IFn
  (invoke [_ v] (forward-fn v))
  (invoke [_ v interval?] (forward-fn v interval?)))

(defn scale->map
  [^ScaleType scale]
  (assoc (.data scale)
         :kind (.kind scale)
         :domain (.domain scale)
         :range (.range scale)))

(defn strip-keys [m] (dissoc m :domain :range :kind))

(defmulti scale (fn [k & _] k))
(defmulti ticks (fn [k & _] k))
(defmulti nice (fn [k _] k))


