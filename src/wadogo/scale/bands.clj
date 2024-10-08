(ns wadogo.scale.bands
  "Creates sequence of bands for given range and padding.

  Bands are evenly distributed intervals with padding.

  Each band is a map with following keys:

  * rstart - interval start
  * rend - interval end
  * point - selected point (default: midpoint)
  * value - domain value

  Input parameters are:

  * bands - number of the bands (default: 1) or sequence of values
  * padding-in - padding between bands (default: 0.0)
  * padding-out - border padding (default: 0.0)
  * align - position of the selected point (0.0 - left, 1.0 - right, 0.5 - midpoint, default)

  Padding is calculated the same way as in `d3`. It's a proportion of the step."
  (:require [fastmath.core :as m]

            [wadogo.common :refer [scale ->ScaleType strip-keys merge-params]]
            [wadogo.utils :refer [build-seq ->extent]]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- bands-inverse-fn
  "Inverse function for bands."
  [bands lst]
  (fn [^double v]
    (loop [s (map vector bands lst)]
      (when (seq s)
        (let [[band-id {:keys [^double start ^double end]}] (first s)]
          (if (<= start v end) band-id (recur (next s))))))))

(defn- decode-align
  [align bands-no]
  (cond
    (number? align) (repeat bands-no (m/constrain ^double align 0.0 1.0))
    (= :spread align) (m/slice-range bands-no)
    (sequential? align) (take bands-no (cycle align))
    :else (repeat bands-no 0.5)))

(defmethod scale :bands
  ([_] (scale :bands {}))
  ([s params]
   (let [params (merge-params s params)

         [^double rstart ^double rend] (->extent (:range params))
         rdiff (- rend rstart)
         norm (m/make-norm 0.0 1.0 rstart rend)

         b (:domain params)
         [^long bands-no bands] (build-seq b)
         bands-no (int bands-no)

         {:keys [^double padding-in ^double padding-out align]} params
         padding-in (m/constrain ^double padding-in 0.0 1.0)
         align (decode-align align bands-no)
         step (/ #_(+ (* bands-no (- 1.0 padding-in))
                      (+ padding-out padding-out)
                      (* (dec bands-no) padding-in))
                 (+ bands-no (* 2.0 padding-out) (- padding-in)))
         nstart (* step padding-out)
         size (* step (- 1.0 padding-in))
         
         lst (for [[^long i ^double offset] (map vector (range bands-no) align)
                   :let [lstart (+ nstart (* i step))
                         lend (+ lstart size)
                         [lstart lend] (if (neg? step) [lend lstart] [lstart lend])]]
               {:value (nth bands i)
                :rstart (norm lstart)
                :rend (norm lend)
                :point (norm (m/lerp lstart lend offset))})
         forward (zipmap bands lst)]
     
     (->ScaleType :bands bands [rstart rend] (:ticks params) (:formatter params)
                  (fn local-forward
                    ([v] (local-forward v false))
                    ([v interval?]
                     (let [res (forward v)]
                       (if interval? res (:point res)))))
                  (bands-inverse-fn  bands lst)
                  (assoc (strip-keys params)
                         :bandwidth (* rdiff (m/abs size))
                         :step (* rdiff (m/abs step))
                         :bands lst)))))

(m/unuse-primitive-operators)
