(ns wadogo.ticks.pretty-datetime-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.ticks.pretty-datetime :as sut]
            [wadogo.oracle :refer [load-oracle]])
  (:import [java.time LocalDateTime]))

;; oracle: R `pretty()` on UTC POSIXct (utils/pretty/pretty_oracle.R)
(def datetime-cases (load-oracle "pretty_datetime.edn"))

(defn- ldt [s] (LocalDateTime/parse s))

(deftest pretty-datetime-oracle-test
  (is (>= (count datetime-cases) 60))
  (doseq [{:keys [start end n inside pretty]} datetime-cases
          :let [start (ldt start) end (ldt end)]]
    (testing [(str start) (str end) n]
      (is (= (map ldt inside) (sut/pretty-datetime-ticks start end n)))
      (is (= (map ldt inside) (reverse (sut/pretty-datetime-ticks end start n))))
      (is (= [(ldt (first pretty)) (ldt (last pretty))]
             (take 2 (sut/pretty-datetime-nice-bounds start end n (sut/default-min-n n))))))))

(deftest pretty-datetime-edges-test
  (let [t (LocalDateTime/of 2012 5 5 10 0 0)]
    (testing "degenerate domain: the point itself if it's a tick"
      (is (= [t] (sut/pretty-datetime-ticks t t 5))))
    (testing "zero ticks requested"
      (is (vector? (sut/pretty-datetime-ticks t (.plusDays t 3) 0))))
    (testing "validation"
      (is (thrown? clojure.lang.ExceptionInfo (sut/pretty-datetime-ticks t (.plusDays t 3) -2)))
      (is (thrown? clojure.lang.ExceptionInfo (sut/pretty-datetime-ticks t (.plusDays t 3) 5 6))))))
