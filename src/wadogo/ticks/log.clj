(ns wadogo.ticks.log
  (:require [fastmath.core :as m]

            [wadogo.ticks.linear :refer [linear-ticks]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- logp 
  [^double base]
  (cond
    (m/one? base) identity
    (== base m/E) (fn ^double [^double x] (m/log x))
    (== base 2.0) m/log2
    (== base 10.0) (fn ^double [^double x] (m/log10 x))
    :else (fn ^double [^double x] (m/logb base x))))

(defn- powp
  [^double base]
  (cond
    (m/one? base) identity
    (== base m/E) (fn ^double [^double x] (m/exp x))
    :else (fn ^double [^double x] (m/pow base x))))

(defn log-ticks
  [^double start ^double end cnt ^double base]
  (let [negative? (neg? start)
        logs (logp base)
        pows (powp base)
        ^double lstart (logs (m/abs start))
        ^double lend (logs (m/abs end))
        ^long c (or cnt (max 1 (- lend lstart)))]
    (map (fn [^double v]
           (let [^double pv (pows v)]
             (if negative? (- pv) pv))) (linear-ticks lstart lend c))))

(m/unuse-primitive-operators)
