(ns wadogo.scale.ordinal
  "`:ordinal` kind: discrete domain values mapped to discrete range values by position (d3 `scaleOrdinal`).

  Attributes:

  - `:domain`: the domain values (duplicates removed); when empty or missing, the positions `0..n-1` of the range values.
  - `:range`: the range values; when empty or missing, the positions `0..n-1` of the domain values. A range shorter than the domain is cycled.
  - `:sort?` (default `true`): how the domain is ordered: `true` sorts with `compare`, a function is used as the comparator of `sort`, a falsy value keeps the given order (needed for values of mixed types).
  - `:unknown` (default `nil`): returned for values outside the domain.

  With an empty domain and range every value is unknown. The second argument of the scale is ignored. The inverse returns the first domain value mapped to a range value (`nil` for other values). Ticks are the domain values; `nice` returns the scale unchanged.

  Throws `ex-info` with `{:domain domain}` when `:sort?` is `true` and the domain values can't be compared (for example numbers and keywords mixed). The implicit domain growth of d3 isn't supported (scales are immutable)."
  (:require [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]))

(defn- ensure-seq-content
  "`a` as a vector, or the positions `0..n-1` of `b` when `a` is empty and `b` isn't."
  [a b]
  (vec (if (and (not (seq a)) (seq b))
         (range (count b))
         a)))

(defn- sort-domain
  [domain sort?]
  (cond
    (fn? sort?) (sort sort? domain)
    sort? (try (sort domain)
               (catch ClassCastException e
                 (throw (ex-info "Ordinal domain values can't be compared, set `:sort?` to `false` or a comparator." {:domain domain} e))))
    :else domain))

(defmethod scale :ordinal
  ([_] (scale :ordinal {}))
  ([s params]
   (let [{:keys [domain range sort? unknown] :as params} (merge-params s params)
         ndomain (-> domain distinct (ensure-seq-content range) (sort-domain sort?) vec)
         nrange (ensure-seq-content range ndomain)
         forward (if (seq nrange)
                   (zipmap ndomain (cycle nrange))
                   {})
         inverse (reduce (fn [m d]
                           (let [r (forward d)]
                             (if (contains? m r) m (assoc m r d)))) {} ndomain)]
     (->ScaleType :ordinal ndomain nrange (:ticks params) (:formatter params)
                  (fn ([v] (forward v unknown))
                    ([v _] (forward v unknown)))
                  inverse
                  (strip-keys params)))))
