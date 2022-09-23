(ns wadogo.config
  (:require [java-time :as dt]))

(def default-params
  {:bands {:domain 1
           :range [0.0 1.0]
           :padding-in 0.0
           :padding-out 0.0
           :align 0.5}
   :datetime {:domain (let [ldt (dt/local-date-time)]
                        [(dt/minus ldt (dt/years 1)) ldt])
              :range [0.0 1.0]}
   :histogram {:range :default}
   :interpolated {:domain [0.0 0.5 1.0]
                  :range [0.0 0.5 1.0]
                  :interpolator :linear
                  :interpolator-params nil}
   :linear {:domain [0.0 1.0]
            :range [0.0 1.0]}
   :log {:domain [1.0 10.0]
         :range [0.0 1.0]
         :base 10.0}
   :pow {:domain [0.0 1.0]
         :range [0.0 1.0]
         :exponent 0.5}
   :quantile {:range 4
              :estimation-strategy :legacy}
   :quantize {:domain [0.0 1.0]
              :range [0]}
   :symlog {:domain [0.0 1.0]
            :range [0.0 1.0]
            :base 10.0}
   :threshold {:domain []}})
