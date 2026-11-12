(ns wadogo.scale.sequential
  "`:sequential` kind: a continuous numerical domain normalised to `[0, 1]` and passed to an interpolator function, typically a color gradient (d3 `scaleSequential`, `scaleSequentialLog`, `scaleSequentialPow`, `scaleSequentialSymlog`).

  Attributes:

  - `:domain`: two numbers (default `[0.0 1.0]`, `[1.0 10.0]` for the `:log` transform); a longer sequence is reduced to its minimum and maximum.
  - `:interpolator` (default `identity`): a function of a number from `[0, 1]` (outside it for values outside the domain, unless clamped), for example a `clojure2d.color/gradient`.
  - `:transform` (default `:linear`): how the domain is normalised: `:linear`, `:log` (as the `:log` kind), `:pow` (as the `:pow` kind) or `:symlog` (as the `:symlog` kind).
  - `:exponent` (default `1.0`): exponent of the `:pow` transform.
  - `:C` (default `1.0`): constant of the `:symlog` transform.
  - `:base` (default `10.0`): base of the log ticks of the `:log` transform.
  - `:clamp`, `:unknown`: as for the `:linear` kind; `:round` rounds the interpolator result, which then has to be a number.

  The range is `[(interpolator 0) (interpolator 1)]` and the `:range` attribute is ignored. The inverse maps a normalised value from `[0, 1]` (not an interpolator result) back to the domain.

  Ticks and `nice` are those of the `:log` kind for the `:log` transform and linear otherwise. A degenerate domain behaves as for the `:linear` kind.

  Throws `ex-info` with `{:interpolator interpolator}` when the interpolator isn't a function, with `{:transform transform}` for an unknown transform and, for the `:log` transform, with `{:domain domain}` when the domain contains or crosses zero."
  (:require [fastmath.core :as m]
            [wadogo.common :refer [scale continuous-scale strip-keys merge-params]]
            [wadogo.utils :refer [->extent make-norm]]
            [wadogo.scale.log :refer [log-transform validate-log-domain]]
            [wadogo.scale.pow :refer [pow-transform]]
            [wadogo.scale.symlog :refer [symlog-transform]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ^:no-doc transform-pair
  "Internal. `[forward inverse]` of the `:transform` in `params` for a domain from `dstart` to `dend`.

  Throws `ex-info` for an unknown transform or a log domain containing or crossing zero."
  [{:keys [transform exponent C] :as params} ^double dstart ^double dend]
  (case transform
    :linear [identity identity]
    :log (do (validate-log-domain (:domain params) dstart dend)
             (log-transform dstart))
    :pow (pow-transform exponent)
    :symlog (symlog-transform C)
    (throw (ex-info (str "Unknown transform: " transform) {:transform transform}))))

(defn ^:no-doc validate-interpolator
  "Internal. `interpolator` when it's a function, otherwise throws `ex-info` with `{:interpolator interpolator}`."
  [interpolator]
  (if (fn? interpolator)
    interpolator
    (throw (ex-info "Interpolator should be a function." {:interpolator interpolator}))))

(defn ^:no-doc transformed-params
  "Internal. `params` over the defaults of the kind `kind` for its `:transform`, with a default domain depending on the transform."
  [kind params log-domain]
  (let [params (merge-params kind params)]
    (if (and (= :log (:transform params)) (not (contains? params :domain)))
      (assoc params :domain log-domain)
      (merge {:domain (if (= kind :diverging) [-1.0 0.0 1.0] [0.0 1.0])} params))))

(defmethod scale :sequential
  ([_] (scale :sequential {}))
  ([s params]
   (let [params (transformed-params s params [1.0 10.0])
         interpolator (validate-interpolator (:interpolator params))
         [dstart dend] (->extent (:domain params))
         [forward inverse] (transform-pair params dstart dend)
         fnorm (make-norm (forward dstart) (forward dend) 0.0 1.0)
         inorm (make-norm 0.0 1.0 (forward dstart) (forward dend))]
     (continuous-scale :sequential [dstart dend] [(interpolator 0.0) (interpolator 1.0)] params
                       (fn [^double v] (interpolator (fnorm (forward v))))
                       (fn ^double [^double t] (inverse (inorm t)))
                       (strip-keys params)))))

(m/unuse-primitive-operators)
