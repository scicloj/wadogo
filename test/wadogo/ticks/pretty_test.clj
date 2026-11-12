(ns wadogo.ticks.pretty-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.ticks.pretty :as sut]
            [wadogo.oracle :refer [load-oracle close? close-seqs?]]))

;; oracle: R `.pretty(bounds = FALSE)` and the multiples ns..nu of unit inside the domain (utils/pretty/pretty_oracle.R)
(def linear-cases (load-oracle "pretty_linear.edn"))

(deftest pretty-spec-oracle-test
  (is (>= (count linear-cases) 300))
  (doseq [{:keys [lo up n min-n unit ns nu]} linear-cases]
    (is (= [(double unit) (double ns) (double nu)] (sut/pretty-spec lo up n min-n))
        (str [lo up n min-n]))))

(deftest pretty-ticks-oracle-test
  (doseq [{:keys [lo up n min-n inside]} linear-cases]
    (testing [lo up n min-n]
      (is (close-seqs? inside (sut/pretty-ticks lo up n min-n)))
      (testing "reversed domain gives reversed ticks"
        (is (close-seqs? (reverse inside) (sut/pretty-ticks up lo n min-n)))))))

(deftest pretty-nice-bounds-oracle-test
  (doseq [{:keys [lo up n min-n unit ns nu]} linear-cases
          :let [[a b u] (sut/pretty-nice-bounds lo up n min-n)]]
    (is (and (close? (* ns unit) a) (close? (* nu unit) b) (== unit u)) (str [lo up n min-n]))))

(deftest pretty-ticks-edges-test
  (testing "issue #6 example"
    (is (= [0.0 50.0 100.0 150.0] (sut/pretty-ticks 0 150 5))))
  (testing "domain end equal to a tick up to rounding is kept"
    (is (= [0.5 0.52 0.54 0.56 0.58 0.6] (sut/pretty-ticks 0.5 0.6 5)))
    (is (= [-9443930.35 -9443930.3 -9443930.25 -9443930.2 -9443930.15 -9443930.1 -9443930.05]
           (sut/pretty-ticks -9443930.35 -9443930.05 7))))
  (testing "decimal ticks are the closest doubles"
    (is (= [0.0 0.1 0.2 0.3] (sut/pretty-ticks 0 0.3 3))))
  (testing "degenerate domains"
    (is (= [2.0] (sut/pretty-ticks 2 2 5)))
    (is (= [0.0] (sut/pretty-ticks 0 0 5)))
    (is (= [0.0 1.0] (sut/pretty-ticks 0 1 0))))
  (testing "non-finite ends"
    (is (= [] (sut/pretty-ticks ##-Inf 1 5)))
    (is (= [] (sut/pretty-ticks 0 ##NaN 5)))))

(deftest pretty-params-validation-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-negative" (sut/pretty-ticks 0 1 -1)))
  (is (= {:ticks 2.5} (try (sut/pretty-ticks 0 1 2.5) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (= {:min-n 6 :ticks 5} (try (sut/pretty-ticks 0 1 5 6) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (thrown? clojure.lang.ExceptionInfo (sut/pretty-ticks 0 1 5 -1)))
  (is (seq (sut/pretty-ticks 0 1 5 5))))
