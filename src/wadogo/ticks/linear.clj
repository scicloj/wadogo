(ns wadogo.ticks.linear
  (:require [fastmath.core :as m]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^:private ^double e10 (m/sqrt 50.0))
(def ^:const ^:private ^double e5 (m/sqrt 10.0))
(def ^:const ^:private ^double e2 m/SQRT2)

(defn step-mult
  "Step multiplier"
  ^double [^double error]
  (cond
    (>= error e10) 10.0
    (>= error e5) 5.0
    (>= error e2) 2.0
    :else 1.0))

(defn- tick-increment
  ^double [^double start ^double end ^long count]
  (let [step (/ (- end start) (max 0 count))
        power (m/floor (m/log10 step))
        p10 (m/pow 10.0 power)
        error (/ step p10)]
    (if (>= power 0.0)
      (* p10 (step-mult error))
      (/ (- (m/pow 10.0 (- power))) (step-mult error)))))

(defn linear-ticks
  [^double start ^double end ^long count]
  (if (and (== start end) (pos? count))
    [start]
    (let [reverse? (< end start)
          [^double start ^double end] (if reverse? [end start] [start end])
          step (tick-increment start end count)
          res (cond
                (or (zero? step)
                    (m/invalid-double? step)) []
                (pos? step) (let [start (m/ceil (/ start step))
                                  end (m/floor (/ end step))
                                  n (m/ceil (inc (- end start)))]
                              (map #(* (+ start ^double %) step) (range n)))
                :else (let [start (m/floor (* start step))
                            end (m/ceil (* end step))
                            n (m/ceil (inc (- start end)))]
                        (map #(/ (- start ^double %) step) (range n))))]
      (if reverse? (reverse res) res))))
