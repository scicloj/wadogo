(ns wadogo.ticks.pretty
  "Ticks based on R's `pretty()` (`src/appl/pretty.c`), with `pretty.default` parameters.

  Unlike `pretty()`, the domain is never extended: only ticks inside the domain are returned.
  Extension to the outer ticks is done by `pretty-nice-bounds` (used by `nice`)."
  (:require [fastmath.core :as m]
            [wadogo.utils :refer [exp10]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; pretty.default parameters
(def ^:private ^:const high-u-bias 1.5)
(def ^:private ^:const u5-bias (+ 0.5 (* 1.5 high-u-bias)))
(def ^:private ^:const shrink-sml 0.75)
(def ^:private ^:const f-min (m/pow 2.0 -20.0))
;; compatible with R's seq*() and pretty()
(def ^:private ^:const rounding-eps 1.0e-10)
;; values closer to zero than this fraction of the step are zero (pretty.default's zapping)
(def ^:private ^:const zap-eps 1.0e-14)
;; MAX_F in pretty.c
(def ^:private ^:const max-cell (/ Double/MAX_VALUE 1.25))

(defn default-min-n
  "Returns the default `min.n` of R's `pretty()` for `n` ticks: `n %/% 3`, the minimal number of intervals."
  ^long [^long n]
  (quot n 3))

(defn- small-interval?
  "Is `[lo, up]` too small relative to its magnitude to be divided into `ndiv` cells (pretty.c `i_small`)."
  [^double lo ^double up ^long ndiv]
  (let [dx (- up lo)]
    (or (and (zero? dx) (zero? up))
        (let [U (* (inc (if (>= u5-bias (+ 0.5 (* 1.5 high-u-bias)))
                          (/ (inc high-u-bias))
                          (/ 1.5 (inc u5-bias))))
                   (max 1 ndiv) m/MACHINE-EPSILON2)] ;; DBL_EPSILON
          (< dx (* (max (m/abs lo) (m/abs up)) U 3.0))))))

(defn- cell-size
  "Initial cell (approximate step) for `lo <= up`, pretty.c's `cell` before the unit selection."
  ^double [^double lo ^double up ^long ndiv ^long min-n]
  (let [dx (- up lo)
        cell (double
              (if (small-interval? lo up ndiv)
                (let [cell (if (and (zero? dx) (zero? up)) 1.0 (max (m/abs lo) (m/abs up)))
                      cell (if (> cell 10.0) (+ 9.0 (/ cell 10.0)) cell)
                      cell (* cell shrink-sml)]
                  (if (> min-n 1) (/ cell min-n) cell))
                (cond
                  (m/valid-double? dx) (if (> ndiv 1) (/ dx ndiv) dx)
                  (< ndiv 2) dx
                  :else (- (/ up ndiv) (/ lo ndiv)))))
        subsmall (let [s (* f-min Double/MIN_NORMAL)] (if (zero? s) Double/MIN_NORMAL s))]
    (cond
      (< cell subsmall) subsmall
      (> cell max-cell) max-cell
      :else cell)))

(defn- cell->unit
  "Select unit from {1,2,5,10}*base closest to `cell`, favouring larger units (pretty.c)."
  ^double [^double cell]
  (let [base (exp10 (m/floor (m/log10 cell)))
        u2 (* 2.0 base)
        u5 (* 5.0 base)
        u10 (* 10.0 base)]
    (if (< (- u2 cell) (* high-u-bias (- cell base)))
      (if (< (- u5 cell) (* u5-bias (- cell u2)))
        (if (< (- u10 cell) (* high-u-bias (- cell u5))) u10 u5)
        u2)
      base)))

(defn pretty-spec
  "R's `.pretty(c(lo, up), n, min.n, bounds = FALSE)`: returns `[unit ns nu]`, the step and the indices of the outer ticks covering `[lo, up]`.

  Preconditions: `lo <= up`, both finite, `0 <= min-n <= n`."
  [^double lo ^double up ^long n ^long min-n]
  (let [unit (cell->unit (cell-size lo up n min-n))
        ns (double (loop [ns (m/floor (+ (/ lo unit) rounding-eps))]
                     (if (> (* ns unit) (+ lo (* rounding-eps unit))) (recur (dec ns)) ns)))
        ns (double (loop [ns ns] (if (m/valid-double? (* ns unit)) ns (recur (inc ns)))))
        nu (double (loop [nu (m/ceil (- (/ up unit) rounding-eps))]
                     (if (< (* nu unit) (- up (* rounding-eps unit))) (recur (inc nu)) nu)))
        nu (double (loop [nu nu] (if (m/valid-double? (* nu unit)) nu (recur (dec nu)))))
        k (long (+ 0.5 (- nu ns)))]
    (if (>= k min-n)
      [unit ns nu]
      (let [k (- min-n k)
            half (quot k 2)
            odd (rem k 2)]
        (cond
          (and (zero? lo) (zero? ns) (not (zero? up))) [unit ns (+ nu k)]
          (and (zero? up) (zero? nu) (not (zero? lo))) [unit (- ns k) nu]
          (>= ns 0.0) [unit (- ns half odd) (+ nu half)]
          :else [unit (- ns half) (+ nu half odd)])))))

(defn- tick-value
  "`i`-th multiple of `unit`. Units below 1 are `1/integer`, division gives the closest decimal (0.3, not 0.30000000000000004)."
  ^double [^double unit ^double i]
  (let [v (if (< unit 1.0)
            (/ i (m/rint (/ unit)))
            (* i unit))]
    (if (< (m/abs v) (* zap-eps unit)) 0.0 v)))

(defn- first-index-inside
  "Smallest `i` with tick value not below `lo - tol` (checked on values, indices can be large)."
  ^double [^double unit ^double lo ^double tol]
  (let [i (m/ceil (/ lo unit))
        i (double (loop [i i] (if (>= (tick-value unit (dec i)) (- lo tol)) (recur (dec i)) i)))]
    (loop [i i] (if (< (tick-value unit i) (- lo tol)) (recur (inc i)) i))))

(defn- last-index-inside
  "Largest `i` with tick value not above `up + tol`."
  ^double [^double unit ^double up ^double tol]
  (let [i (m/floor (/ up unit))
        i (double (loop [i i] (if (<= (tick-value unit (inc i)) (+ up tol)) (recur (inc i)) i)))]
    (loop [i i] (if (> (tick-value unit i) (+ up tol)) (recur (dec i)) i))))

(defn validate-pretty-params
  "Throws when `n` is not a non-negative integer or `min-n` is not an integer in `[0, n]` (as R's `pretty()`)."
  [n min-n]
  (when-not (and (integer? n) (not (neg? (long n))))
    (throw (ex-info "Number of ticks for pretty ticks should be a non-negative integer." {:ticks n})))
  (when-not (and (integer? min-n) (<= 0 (long min-n) (long n)))
    (throw (ex-info "`:min-n` for pretty ticks should be an integer in [0, n]." {:min-n min-n :ticks n}))))

(defn- sorted-pair
  [^double start ^double end]
  (if (< end start) [end start true] [start end false]))

(defn pretty-ticks
  "Ticks inside `[start, end]` (any order, ticks follow the order) chosen as R's `pretty(c(start, end), n, min.n)`.

  Returns an empty sequence for non-finite ends."
  ([start end n] (pretty-ticks start end n (default-min-n n)))
  ([^double start ^double end n min-n]
   (validate-pretty-params n min-n)
   (if-not (and (m/valid-double? start) (m/valid-double? end))
     []
     (let [[^double lo ^double up reverse?] (sorted-pair start end)
           [^double unit] (pretty-spec lo up n min-n)
           tol (* rounding-eps unit)
           i0 (first-index-inside unit lo tol)
           i1 (last-index-inside unit up tol)
           res (map (fn [^double i] (tick-value unit i)) (range i0 (inc i1)))]
       (vec (if reverse? (reverse res) res))))))

(defn pretty-nice-bounds
  "Outer pretty ticks covering `[lo, up]` (`lo <= up`), as `pretty()` returns as its range: `[lo' up' unit]`.
  `nil` for non-finite ends or bounds."
  [^double lo ^double up n min-n]
  (validate-pretty-params n min-n)
  (when (and (m/valid-double? lo) (m/valid-double? up))
    (let [[^double unit ^double ns ^double nu] (pretty-spec lo up n min-n)
          a (tick-value unit ns)
          b (tick-value unit nu)]
      (when (and (m/valid-double? a) (m/valid-double? b))
        [a b unit]))))

(m/unuse-primitive-operators)
