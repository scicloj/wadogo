(ns wadogo.ticks.linear
  (:require [fastmath.core :as m]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^:private e10 (m/sqrt 50.0))
(def ^:const ^:private e5 (m/sqrt 10.0))
(def ^:const ^:private e2 m/SQRT2)

(defn step-mult
  "Step multiplier"
  ^double [^double error]
  (cond
    (>= error e10) 10.0
    (>= error e5) 5.0
    (>= error e2) 2.0
    :else 1.0))

(defn- tick-increment
  ^double [^double start ^double end ^double count]
  (let [step (/ (- end start) (max 0 count))
        power (m/floor (m/log10 step))
        p10 (m/pow 10.0 power)
        error (/ step p10)]
    (if (>= power 0.0)
      (* p10 (step-mult error))
      (/ (- (m/pow 10.0 (- power))) (step-mult error)))))

(defn- js-round
  "JavaScript's `Math.round` (halves up), as used by d3."
  ^double [^double x]
  (m/floor (+ x 0.5)))

(defn linear-ticks
  "Ticks inside `[start, end]` (any order), d3 style, for an approximate (possibly fractional) `count`.

  Tick indices are rounded and checked against the domain (d3-array 3.2), so a domain end equal to a tick is kept."
  [^double start ^double end ^double count]
  (if (and (== start end) (pos? count))
    [start]
    (let [reverse? (< end start)
          [^double start ^double end] (if reverse? [end start] [start end])
          step (tick-increment start end count)
          res (cond
                (or (zero? step)
                    (m/invalid-double? step)) []
                (pos? step) (let [i1 (js-round (/ start step))
                                  i2 (js-round (/ end step))
                                  i1 (if (< (* i1 step) start) (inc i1) i1)
                                  i2 (if (> (* i2 step) end) (dec i2) i2)]
                              (map (fn [^double i] (* i step)) (range i1 (inc i2))))
                :else (let [inc-step (- step)
                            i1 (js-round (* start inc-step))
                            i2 (js-round (* end inc-step))
                            i1 (if (< (/ i1 inc-step) start) (inc i1) i1)
                            i2 (if (> (/ i2 inc-step) end) (dec i2) i2)]
                        (map (fn [^double i] (/ i inc-step)) (range i1 (inc i2)))))]
      (if reverse? (reverse res) res))))

(defn linear-nice-bounds
  "`[lo, up]` (`lo < up`) extended to multiples of the tick step for `count` ticks (d3 `nice`): `[lo' up' step]`.
  `nil` when there is no valid step or the extended bounds are not finite."
  [^double lo ^double up ^double count]
  (let [step (tick-increment lo up count)]
    (when-not (or (zero? step) (m/invalid-double? step))
      (let [[^double a ^double b] (if (pos? step)
                                    [(* (m/floor (/ lo step)) step) (* (m/ceil (/ up step)) step)]
                                    [(/ (m/ceil (* lo step)) step) (/ (m/floor (* up step)) step)])]
        (when (and (m/valid-double? a) (m/valid-double? b))
          [a b step])))))

(m/unuse-primitive-operators)
