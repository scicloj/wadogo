(ns wadogo.format.numbers-test
  (:require [wadogo.format.numbers :refer [formatter]]
            [midje.sweet :refer [facts fact =>]]))

(def a [0.000001 0.00001 0.0001 0.001 0.01 0.1 0.0 1.0 10.0 100.0 1000.0 10000.0 100000.0])
(def b [10.0 10.1 10.11 10.111 10.1111 10.11111 1.0 1.1 1.11 1.111 1.1111 1.11111 0.0 0.1 0.11 0.111 0.1111 -0.11111])
(def c (range -5 4 0.8795833))
(def d [-1.0e-20 -1.334e-100 3.43e100 4.556e20 1.0e-20 1.334e-100 -3.43e100 -41.556e20 0.999e-300 -0.999e300])
(def e [-1.0e99 1.0e99])
(def f [-1.0e100 1.0e100])
(def g [0.002 0.0002 0.000333 0.1 -0.0003 0.0])
(def h [0.002 0.0002 0.00333 0.00001 -0.0003 0.022 0.0001])
(def i [10.0 ##NaN ##Inf ##-Inf 100 0.001 nil])
(def j (map float [39.81 36.35 43.22 28.37 25.45 -39.81 36.351 43.221 28.371 25.451]))

(map str a)

(defn format-sequence
  ([xs] (map (formatter) xs))
  ([xs m] (map (formatter m) xs)))

(facts "formatting sequences"

  (fact "works with float"
    (format-sequence j {:digits 3}) => '("39.810" "36.350" "43.220" "28.370" "25.450" "-39.810" "36.351" "43.221" "28.371" "25.451"))

  (fact "works with invalid doubles"
    (format-sequence i) => '("10.0" "NaN" "∞" "-∞" "100.0" "0.001" "NA")
    (format-sequence i {:digits -1 :threshold 0}) => '("1.0E+01" "NaN" "∞" "-∞" "1.0E+02" "1.0E-03" "NA"))

  (fact "works with various exponents"
    (format-sequence a) => '("0.000001" "0.00001" "0.0001" "0.001" "0.01" "0.1" "0.0" "1.0" "10.0" "100.0" "1000.0" "10000.0" "100000.0"))

  (fact "creates scientific representation"
    (format-sequence a {:digits -5 :threshold 0}) => '("1.0E-06" "1.0E-05" "1.0E-04" "1.0E-03" "1.0E-02" "1.0E-01" "0.0E+00" "1.0E+00" "1.0E+01" "1.0E+02" "1.0E+03" "1.0E+04" "1.0E+05"))

  (fact "works with negative values"
    (format-sequence b) => '("10.0" "10.1" "10.11" "10.111" "10.1111" "10.11111" "1.0" "1.1" "1.11" "1.111" "1.1111" "1.11111" "0.0" "0.1" "0.11" "0.111" "0.1111" "-0.11111"))

  (fact "creates scientific representation with negative values"
    (format-sequence b {:digits 5 :threshold 0}) => '("1.00000E+01" "1.01000E+01" "1.01100E+01" "1.01110E+01" "1.01111E+01" "1.01111E+01" "1.00000E+00" "1.10000E+00" "1.11000E+00" "1.11100E+00" "1.11110E+00" "1.11111E+00" "0.00000E+00" "1.00000E-01" "1.10000E-01" "1.11000E-01" "1.11100E-01" "-1.11110E-01"))
  
  (fact "rounds"
    (format-sequence c {:digits -8}) => '("-5.0" "-4.1204167" "-3.2408334" "-2.3612501" "-1.4816668" "-0.6020835" "0.2774998" "1.1570831" "2.0366664" "2.9162497" "3.795833")
    (format-sequence c {:digits 4}) => '("-5.0000" "-4.1204" "-3.2408" "-2.3613" "-1.4817" "-0.6021" "0.2775" "1.1571" "2.0367" "2.9162" "3.7958")
    (format-sequence c {:digits 4 :threshold 0}) => '("-5.0000E+00" "-4.1204E+00" "-3.2408E+00" "-2.3613E+00" "-1.4817E+00" "-6.0208E-01" "2.7750E-01" "1.1571E+00" "2.0367E+00" "2.9162E+00" "3.7958E+00")
    (format-sequence c {:digits 2}) => '("-5.00" "-4.12" "-3.24" "-2.36" "-1.48" "-0.60" "0.28" "1.16" "2.04" "2.92" "3.80"))

  (fact "works with high exponents"
    (format-sequence d {:digits 4}) => '("-1.0000E-20" "-1.3340E-100" "3.4300E+100" "4.5560E+20" "1.0000E-20" "1.3340E-100" "-3.4300E+100" "-4.1556E+21" "9.9900E-301" "-9.9900E+299")
    (format-sequence e) => '("-1.0E+99" "1.0E+99")
    (format-sequence f) => '("-1.0E+100" "1.0E+100"))
  
  (fact "works with low exponents"
    (format-sequence g) => '("0.002" "0.0002" "0.000333" "0.1" "-0.0003" "0.0")
    (format-sequence h) => '("0.002" "0.0002" "0.00333" "0.00001" "-0.0003" "0.022" "0.0001")))
