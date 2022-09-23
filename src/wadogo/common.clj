(ns wadogo.common
  (:refer-clojure :exclude [range])
  (:require [fastmath.core :as m]

            [wadogo.config :as config]
            
            [wadogo.ticks.linear :as tlinear]
            [wadogo.ticks.log :as tlog]
            [wadogo.ticks.datetime :as tdatetime]

            [wadogo.format.numbers :as fnumbers]
            [wadogo.format.datetime :as fdatetime]
            [wadogo.format.ranges :as franges])
  (:import [clojure.lang IFn]))

(def ^:private mappings {:c->c [:continuous :continuous]
                         :c->d [:continuous :discrete]
                         :d->c [:discrete :continuous]
                         :d->d [:discrete :discrete]
                         :dt->c [:datetime :continuous]})

(defonce mapping
  {:linear (mappings :c->c)
   :interpolated (mappings :c->c)
   :log (mappings :c->c)
   :symlog (mappings :c->c)
   :pow (mappings :c->c)
   :bands (mappings :d->c)
   :ordinal (mappings :d->d)
   :quantile (mappings :c->d)
   :quantize (mappings :c->d)
   :threshold (mappings :c->d)
   :histogram (mappings :c->d)
   :datetime (mappings :dt->c)
   :constant (mappings :d->d)})

(deftype ScaleType [kind domain range ticks formatter forward-fn inverse-fn data]
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
         :range (.range scale)
         :ticks (.ticks scale)
         :formatter (.formatter scale)))

(defn strip-keys [m] (dissoc m :domain :range :kind :ticks :formatter))

;;

(defmulti scale (fn [k & _] k))

;; ticks

(defn-  kind->dispatch
  [^ScaleType scale]
  (let [kind (.kind scale)
        [d r] (mapping kind)]
    (if (and d r)
      (cond
        (= :log kind) :log
        (and (= d :continuous)
             (= r :discrete)) :discrete
        (= d :discrete) :ordinal
        (= d :datetime) :datetime
        :else :linear)
      kind)))

(defmulti ticks (fn [scale] (kind->dispatch scale)))

(defn discrete->ticks
  [tcks data]
  (let [t (or tcks data)]
    (if-not (number? t) t
            (take-nth (max 1 (m/round (/ (count data) (double t)))) data))))

(defmethod ticks :discrete [^ScaleType scale]
  (discrete->ticks (.ticks scale) (.range scale)))

(defmethod ticks :ordinal [^ScaleType scale]
  (discrete->ticks (.ticks scale) (.domain scale)))

(defmethod ticks :linear [^ScaleType scale]
  (let [t (or (.ticks scale) 10)]
    (if-not (number? t) t
            (let [start (first (.domain scale))
                  end (last (.domain scale))]
              (tlinear/linear-ticks start end t)))))

(defmethod ticks :log [^ScaleType scale]
  (let [t (or (.ticks scale) -1)]
    (if-not (number? t) t
            (let [[start end] (.domain scale)]
              (tlog/log-ticks start end (if (neg? t) nil t) (:base (.data scale)))))))

(defmethod ticks :datetime [^ScaleType scale]
  (let [t (or (.ticks scale) 10)]
    (if-not (number? t) t
            (let [[start end] (.domain scale)]
              (tdatetime/datetime-ticks start end (:millis (.data scale)) t)))))

;;

(defn- formatter->dispatch
  [^ScaleType scale tcks]
  (let [formatter (.formatter scale)
        kind (.kind scale)
        [d _] (mapping kind)
        stcks (remove nil? tcks)]
    (cond
      (fn? formatter) :fn
      (= d :datetime) :datetime
      (every? #(and (sequential? %)
                    (= 2 (count %))) stcks) :ranges
      (and (every? number? stcks)
           (some #(or (double? %) (float? %)) tcks)) :doubles
      (and stcks (every? int? stcks)) :ints
      :else :default)))

(defmulti formatter (fn [scale tcks] (formatter->dispatch scale tcks)))

(defmethod formatter :default [_ _] str)
(defmethod formatter :fn [^ScaleType scale _] (.formatter scale))
(defmethod formatter :datetime [_ tcks] (fdatetime/time-format tcks))
(defmethod formatter :ints [^ScaleType scale _] (fnumbers/int-formatter (:formatter-params (.data scale))))
(defmethod formatter :doubles [^ScaleType scale _] (fnumbers/formatter (:formatter-params (.data scale))))
(defmethod formatter :ranges [^ScaleType scale _] (franges/range-formatter {:formatter-params (.data scale)}))

;; params

(defn merge-params
  [scale-key params]
  (merge (config/default-params scale-key) params))

(defn log-params
  [{:keys [domain base] :as params}]
  (let [[x1 x2] domain]
    (if (and base (not x2))
      (assoc params :domain [(or x1 1.0) base])
      params)))
