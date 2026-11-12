(ns wadogo.scale.log
  "`:log` kind: logarithmic mapping of a positive or negative numerical domain to a continuous numerical range (d3 `scaleLog`).

  Attributes:

  - `:domain` (default `[1.0 10.0]`): two numbers of the same sign; a longer sequence is reduced to its minimum and maximum. A one-value domain `[x]` together with `:base` becomes `[x base]`.
  - `:range` (default `[0.0 1.0]`).
  - `:base` (default `10.0`): base of the logarithm, greater than `1.0`, used only by ticks and `nice` (the mapping doesn't depend on it).
  - `:clamp`, `:unknown`, `:round`: as for the `:linear` kind.

  Forward is linear in the logarithm of the absolute value; values of the other sign than the domain (and zero) give `NaN`.

  Ticks are powers of the base and their subdivisions (d3 style, one tick per power by default) or, with `:ticks-method :pretty`, the grid with the tick count closest to the requested one (5 by default). `nice` extends the domain to the outer ticks.

  A domain containing or crossing zero throws `ex-info` with `{:domain domain}`. A degenerate domain behaves as for the `:linear` kind."
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale continuous-scale strip-keys merge-params log-params]]
            [wadogo.utils :refer [->extent make-norm]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ^:no-doc log-transform
  "Internal. `[forward inverse]` log transform of a domain starting at `dstart`, as d3: `log(x)` for a positive domain, `-log(-x)` for a negative one."
  [^double dstart]
  (if (neg? dstart)
    [(fn ^double [^double v] (- (m/log (- v))))
     (fn ^double [^double v] (- (m/exp (- v))))]
    [(fn ^double [^double v] (m/log v))
     (fn ^double [^double v] (m/exp v))]))

(defn ^:no-doc validate-log-domain
  "Internal. Throws `ex-info` with `{:domain domain}` unless `dstart` and `dend` are non-zero and of the same sign."
  [domain ^double dstart ^double dend]
  (when-not (pos? (* dstart dend))
    (throw (ex-info "Log scale domain can't contain or cross zero." {:domain domain}))))

(defmethod scale :log
  ([_] (scale :log {}))
  ([s params]
   (let [params (merge-params s (log-params params))
         [dstart dend :as domain] (->extent (:domain params))
         _ (validate-log-domain domain dstart dend)
         [rstart rend] (->extent (:range params))
         [forward inverse] (log-transform dstart)
         ls (forward dstart)
         le (forward dend)
         fnorm (make-norm ls le rstart rend)
         inorm (make-norm rstart rend ls le)]
     (continuous-scale :log [dstart dend] [rstart rend] params
                       (fn ^double [^double v] (fnorm (forward v)))
                       (fn ^double [^double v] (inverse (inorm v)))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
