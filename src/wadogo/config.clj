(ns wadogo.config
  "Default attributes of the scale kinds."
  (:require [java-time :as dt]))

(def ^{:doc "Default attributes of every scale kind: a map from the kind to the attribute map merged under the attributes given to `wadogo.scale/scale`. Kinds missing here (`:constant`) or attributes missing in a kind's map have no default; the `:quantile` and `:histogram` kinds need a `:domain`.

  The `:datetime` default domain is the year before the moment this namespace was loaded. See `wadogo.scale/kind-info` for the meaning of the attributes."}
  default-params
  {:bands {:domain 1
           :range [0.0 1.0]
           :padding-in 0.0
           :padding-out 0.0
           :align 0.5
           :align-outer 0.5}
   :point {:domain 1
           :range [0.0 1.0]
           :padding 0.0
           :align-outer 0.5}
   :datetime {:domain (let [ldt (dt/local-date-time)]
                        [(dt/minus ldt (dt/years 1)) ldt])
              :range [0.0 1.0]}
   :diverging {:interpolator identity
               :transform :linear
               :exponent 1.0
               :C 1.0}
   :histogram {:range :freedman-diaconis}
   :identity {:domain [0.0 1.0]}
   :interpolated {:domain [0.0 0.5 1.0]
                  :range [0.0 0.5 1.0]
                  :interpolator :linear
                  :interpolator-params nil}
   :linear {:domain [0.0 1.0]
            :range [0.0 1.0]}
   :log {:domain [1.0 10.0]
         :range [0.0 1.0]
         :base 10.0}
   :ordinal {:sort? true}
   :pow {:domain [0.0 1.0]
         :range [0.0 1.0]
         :exponent 0.5}
   :quantile {:range 4
              :estimation-strategy :r7}
   :quantize {:domain [0.0 1.0]
              :range [0]}
   :radial {:domain [0.0 1.0]
            :range [0.0 1.0]}
   :sequential {:interpolator identity
                :transform :linear
                :exponent 1.0
                :C 1.0}
   :symlog {:domain [0.0 1.0]
            :range [0.0 1.0]
            :C 1.0}
   :threshold {:domain []}})
