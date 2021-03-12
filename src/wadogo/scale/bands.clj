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

            [wadogo.common :refer [scale ->ScaleType strip-keys]]
            [wadogo.utils :refer [build-seq]]))

(set! *warn-on-reflection* true)
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

(def ^:private bands-params
  {:domain 1
   :range [0.0 1.0]
   :padding-in 0.0
   :padding-out 0.0
   :align 0.5})

(defmethod scale :bands
  ([_] (scale :bands {}))
  ([_ params]
   (let [params (merge bands-params params)

         [^double rstart ^double rend] (:range params)
         rdiff (- rend rstart)
         norm (m/make-norm 0.0 1.0 rstart rend)

         b (:domain params)
         [^long bands-no bands] (build-seq b)
         bands-no (int bands-no)

         {:keys [^double padding-in ^double padding-out ^double align]} params
         padding-in (m/constrain ^double padding-in 0.0 1.0)
         align (m/constrain ^double align 0.0 1.0)
         step (/ (+ (* bands-no (- 1.0 padding-in))
                    (+ padding-out padding-out)
                    (* (dec bands-no) padding-in)))
         nstart (* step padding-out)
         size (* step (- 1.0 padding-in))
         
         lst (for [^long i (range bands-no)
                   :let [lstart (+ nstart (* i step))
                         lend (+ lstart size)
                         [lstart lend] (if (neg? step) [lend lstart] [lstart lend])]]
               {:value (nth bands i)
                :rstart (norm lstart)
                :rend (norm lend)
                :point (norm (m/lerp lstart lend align))})
         forward (zipmap bands lst)]
     
     (->ScaleType :bands bands (:range params)
                  (fn local-forward
                    ([v] (local-forward v true))
                    ([v interval?]
                     (let [res (forward v)]
                       (if interval? res (:point res)))))
                  (bands-inverse-fn  bands lst)
                  (assoc (strip-keys params)
                         :bandwidth (* rdiff (m/abs size))
                         :step (* rdiff (m/abs step)))))))
