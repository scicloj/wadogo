(ns wadogo.ticks.log
  (:require [fastmath.core :as m]

            [wadogo.utils :refer [exp10]]
            [wadogo.ticks.linear :refer [linear-ticks linear-nice-bounds]]
            [wadogo.ticks.pretty :refer [pretty-ticks pretty-nice-bounds default-min-n]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; default number of ticks of the linear fallback (as linear scales)
(def ^:private ^:const linear-count 10)

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
    (== base 10.0) exp10
    :else (fn ^double [^double x] (m/pow base x))))

;; exponents of powers checked on values, logarithms of exact powers can be off by an ulp
(def ^:private ^:const exponent-eps 1.0e-10)

(defn- exponent-ceil
  "Smallest integer `e` with `base^e >= x` (positive `x`)."
  ^double [logs pows ^double x]
  (let [e (m/ceil (- ^double (logs x) exponent-eps))]
    (if (< ^double (pows e) x) (inc e) e)))

(defn- exponent-floor
  "Largest integer `e` with `base^e <= x` (positive `x`)."
  ^double [logs pows ^double x]
  (let [e (m/floor (+ ^double (logs x) exponent-eps))]
    (if (> ^double (pows e) x) (dec e) e)))

(defn- snap-to
  "`e` replaced by integer `r` when they are closer than `exponent-eps`."
  ^double [^double e ^double r]
  (if (< (m/abs (- e r)) exponent-eps) r e))

(defn- integer-base? [^double base] (and (> base 1.0) (== base (m/floor base))))

;; sign handling: ticks and bounds are calculated on the positive part, `lo <= hi`

(defn- positive-ticks
  "Apply `f` returning positive ticks for positive `lo <= hi` to the domain `[start end]` of one sign. Ticks follow the domain order."
  [f ^double start ^double end]
  (let [negative? (neg? start)
        a (m/abs start)
        b (m/abs end)
        ticks (f (min a b) (max a b))
        ticks (sort (if negative? (map (fn [^double v] (- v)) ticks) ticks))]
    (vec (if (< end start) (reverse ticks) ticks))))

(defn- positive-bounds
  "Apply `f` returning `[lo' hi' grid]` for positive `lo <= hi` to a sorted domain `[lo hi]` of one sign.
  `nil` when `f` returns `nil` or bounds which are not positive and finite (repeated extension can overflow)."
  [f ^double lo ^double hi]
  (let [negative? (neg? lo)
        bounds (if negative? (f (- hi) (- lo)) (f lo hi))]
    (when-let [[^double a ^double b grid] bounds]
      (when (and (pos? a) (m/valid-double? b))
        (if negative? [(- b) (- a) grid] [a b grid])))))

(defn- keep-positive
  "Linear fallback bounds can't reach zero: the lower end is the largest power of the base not above `lo` then."
  [^double lo ^double base bounds]
  (when-let [[^double a b grid] bounds]
    (let [pows (powp base)]
      [(if (pos? a) a (pows (exponent-floor (logp base) pows lo))) b [:linear grid]])))

;; default ticks: integer powers of the base, number of ticks capped by the number of powers (d3)

(defn- exponent-range
  "For positive `lo <= hi`: logarithms snapped to the nearest inner powers `[le he]`, the same snapped to the nearest outer powers `[lo-e hi-e]`,
  number of powers inside and the capped (fractional, as d3) count of ticks."
  [^double lo ^double hi cnt ^double base]
  (let [logs (logp base)
        pows (powp base)
        l (double (logs lo))
        h (double (logs hi))
        ce (exponent-ceil logs pows lo)
        fe (exponent-floor logs pows hi)
        span (- h l)]
    {:inner [(snap-to l ce) (snap-to h fe)]
     :outer [(snap-to l (exponent-floor logs pows lo)) (snap-to h (exponent-ceil logs pows hi))]
     :powers (inc (- fe ce))
     :count (if cnt (min span (double cnt)) span)}))

(defn log-ticks
  "Default log ticks: powers of the base inside the domain, at most `cnt` (or one per power when `nil`).
  Domains with fewer than two such powers get linear ticks."
  [^double start ^double end cnt ^double base]
  (positive-ticks (fn [^double lo ^double hi]
                    (let [{[le he] :inner :keys [^double powers count]} (exponent-range lo hi cnt base)]
                      (if (< powers 2.0)
                        (linear-ticks lo hi (or cnt linear-count))
                        ;; step is at least one power
                        (map (powp base) (linear-ticks le he count)))))
                  start end))

(defn log-nice-bounds
  "Sorted domain `[lo hi]` extended to the outer default log ticks: `[lo' hi' grid]` (`grid` identifies the tick step), `nil` when not possible."
  [^double lo ^double hi cnt ^double base]
  (positive-bounds (fn [^double lo ^double hi]
                     (let [{[le he] :outer :keys [^double powers count]} (exponent-range lo hi cnt base)]
                       (if (< powers 2.0)
                         (keep-positive lo base (linear-nice-bounds lo hi (or cnt linear-count)))
                         (when-let [[a b step] (linear-nice-bounds le he count)]
                           (let [pows (powp base)]
                             [(pows a) (pows b) [:powers step]])))))
                   lo hi))

;; pretty ticks: the tick grid with number of ticks closest to `n`, candidates are
;; subdivisions of powers (all `1..base-1` multipliers as d3, R's 1-2-5 and 1-5 for base 10) and powers of the base (d3);
;; R pretty linear ticks when there are too few of them (d3's fallback rule)

(defn- multiplier-sets
  "Multipliers of powers of the base used as subdivision grids, densest first."
  [^double base]
  (cond
    (== base 10.0) [(range 1 10) [1 2 5] [1 5]]
    (and (integer-base? base) (> base 2.0)) [(range 1 (long base))]
    :else []))

(defn- subdivision-tick
  "`k * base^e`, divided for negative exponents to get exact decimals (d3)."
  ^double [pows ^long k ^double e]
  (if (neg? e)
    (/ k ^double (pows (- e)))
    (* k ^double (pows e))))

(defn- subdivision-ticks
  "Ticks `k * base^e` inside positive `[lo, hi]` for multipliers `ks`."
  [^double lo ^double hi ks ^double base]
  (let [logs (logp base)
        pows (powp base)]
    (for [e (range (exponent-floor logs pows lo) (inc (exponent-floor logs pows hi)))
          k ks
          :let [t (subdivision-tick pows k e)]
          :when (<= lo t hi)]
      t)))

(defn- power-grid-count
  "Fractional (d3) count of ticks for the exponent grid."
  ^double [^double i ^double j ^long n]
  (min (- j i) (double n)))

(defn- power-ticks
  "Powers of the base inside positive `[lo, hi]`, exponents as d3 linear ticks for capped count (d3)."
  [^double lo ^double hi ^long n ^double base]
  (let [logs (logp base)
        pows (powp base)
        ;; snapped to the inner powers, so the powers equal to domain ends are kept
        i (snap-to (logs lo) (exponent-ceil logs pows lo))
        j (snap-to (logs hi) (exponent-floor logs pows hi))]
    (map pows (linear-ticks i j (power-grid-count i j n)))))

(defn- pretty-log-grid
  "Chosen grid for positive `lo <= hi`: `{:grid [:subdivisions ks] | [:powers] :ticks ...}`, the one with number of ticks closest to `n` (ties: fewer ticks).
  `nil` when linear fallback should be used: the densest grid has fewer than `n/2` ticks."
  [^double lo ^double hi ^long n ^double base]
  (let [candidates (conj (mapv (fn [ks] {:grid [:subdivisions (vec ks)] :ticks (subdivision-ticks lo hi ks base)})
                               (multiplier-sets base))
                         {:grid [:powers] :ticks (power-ticks lo hi n base)})
        counts (map (comp count :ticks) candidates)]
    ;; linear fallback when even the densest grid is too sparse (d3)
    (when-not (< (* 2 ^long (reduce max counts)) n)
      (first (sort-by (juxt #(m/abs (- (count (:ticks %)) n)) #(count (:ticks %))) candidates)))))

(defn pretty-log-ticks
  "Log ticks with number of ticks closest to `n`: from subdivisions of powers of the base (all multipliers `1..base-1`, also 1-2-5 and 1-5 for base 10) or powers of the base, inside the domain.
  When even the densest grid has fewer than `n/2` ticks, R pretty linear ticks are used."
  [^double start ^double end ^long n ^double base]
  (positive-ticks (fn [^double lo ^double hi]
                    (if-let [{:keys [ticks]} (pretty-log-grid lo hi n base)]
                      ticks
                      (pretty-ticks lo hi n)))
                  start end))

(defn pretty-log-nice-bounds
  "Sorted domain `[lo hi]` extended to the nearest outer pretty log ticks (of the chosen grid): `[lo' hi' grid]`, `nil` when not possible."
  [^double lo ^double hi ^long n ^double base]
  (positive-bounds
   (fn [^double lo ^double hi]
     (let [logs (logp base)
           pows (powp base)
           {[kind ks] :grid :as chosen} (pretty-log-grid lo hi n base)]
       (cond
         (nil? chosen) (keep-positive lo base (pretty-nice-bounds lo hi n (default-min-n n)))

         (= kind :subdivisions)
         (let [el (exponent-floor logs pows lo)
               eh (exponent-floor logs pows hi)
               lower (->> (reverse ks)
                          (map #(subdivision-tick pows % el))
                          (filter (fn [^double t] (<= t lo)))
                          (first))
               upper (or (->> ks
                              (map #(subdivision-tick pows % eh))
                              (filter (fn [^double t] (>= t hi)))
                              (first))
                         (subdivision-tick pows 1 (inc eh)))]
           [lower upper [:subdivisions ks]])

         :else (let [i (snap-to (logs lo) (exponent-floor logs pows lo))
                     j (snap-to (logs hi) (exponent-ceil logs pows hi))]
                 (when-let [[^double a ^double b step] (linear-nice-bounds i j (power-grid-count i j n))]
                   [(pows a) (pows b) [:powers step]])))))
   lo hi))

(m/unuse-primitive-operators)
