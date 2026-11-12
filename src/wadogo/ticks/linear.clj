(ns wadogo.ticks.linear
  "d3 style linear ticks (d3-array `ticks`, `tickIncrement`) and d3 `nice` bounds: steps of 1, 2 or 5 times a power of ten, chosen for an approximate number of ticks.

  [[linear-ticks]] returns the ticks inside a domain, [[linear-nice-bounds]] the domain extended to multiples of the step. Used by `:default` ticks of continuous kinds and by log ticks for short domains."
  (:require [fastmath.core :as m]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^:private e10 (m/sqrt 50.0))
(def ^:const ^:private e5 (m/sqrt 10.0))
(def ^:const ^:private e2 m/SQRT2)

(defn- step-mult
  "Multiplier (1, 2, 5 or 10) of a power of ten for the ratio `error` of the raw step to that power."
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
  "Returns d3 style ticks inside the interval from `start` to `end`.

  Parameters:

  - `start`, `end`: the domain ends, in any order (the ticks follow it).
  - `count`: the approximate number of ticks, possibly fractional.

  The step is 1, 2 or 5 times a power of ten, chosen so that the number of ticks is close to `count`; ticks are multiples of the step, computed as integer multiples (or divisions for steps below one) to avoid accumulated rounding. Tick indices are rounded and checked against the domain (d3-array 3.2), so a domain end equal to a tick is kept.

  Returns a sequence of doubles: `[start]` for equal ends and a positive `count`, empty for a non-positive `count` or non-finite ends.

  See also [[linear-nice-bounds]], `wadogo.ticks.pretty/pretty-ticks`."
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
  "Returns the interval from `lo` to `up` extended to the nearest multiples of the tick step, one step of d3 `nice`.

  Parameters:

  - `lo`, `up`: the interval ends, `lo < up`.
  - `count`: the approximate number of ticks, as in [[linear-ticks]].

  Returns `[lo' up' step]`, where `step` is the tick increment of d3 (negative for steps below one: the inverse of the step, negated), which identifies the tick grid; `nil` when there is no valid step or the extended ends aren't finite. `wadogo.scale/nice` repeats it until the step doesn't change.

  See also [[linear-ticks]]."
  [^double lo ^double up ^double count]
  (let [step (tick-increment lo up count)]
    (when-not (or (zero? step) (m/invalid-double? step))
      (let [[^double a ^double b] (if (pos? step)
                                    [(* (m/floor (/ lo step)) step) (* (m/ceil (/ up step)) step)]
                                    [(/ (m/ceil (* lo step)) step) (/ (m/floor (* up step)) step)])]
        (when (and (m/valid-double? a) (m/valid-double? b))
          [a b step])))))

(m/unuse-primitive-operators)
