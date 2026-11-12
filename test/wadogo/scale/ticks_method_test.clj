(ns wadogo.scale.ticks-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [wadogo.scale :as s])
  (:import [java.time LocalDateTime]))

(deftest ticks-method-selection-test
  (testing "default is unchanged"
    (is (= [0.0 20.0 40.0 60.0 80.0 100.0 120.0 140.0] (s/ticks (s/scale :linear {:domain [0 150]}) 5)))
    (is (= (s/ticks (s/scale :linear {:domain [0 150]}))
           (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :default})))))
  (testing "pretty, 5 ticks by default"
    (is (= [0.0 50.0 100.0 150.0] (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty})))))
  (testing "method survives count change and domain change"
    (let [sc (s/scale :linear {:domain [0 150] :ticks-method :pretty})]
      (is (= [0.0 20.0 40.0 60.0 80.0 100.0 120.0 140.0] (s/ticks sc 10)))
      (is (= [0.0 0.2 0.4 0.6 0.8 1.0] (s/ticks (s/with-domain sc [0 1]))))
      (is (= :pretty (s/data (s/with-ticks sc 7) :ticks-method)))))
  (testing ":min-n"
    ;; R: pretty(c(0,150), n=1, min.n=1) => 0 200; pretty(c(0,150), n=3, min.n=3) => 0 50 100 150
    (is (= [0.0] (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty :ticks 1 :ticks-params {:min-n 1}}))))
    (is (= [0.0 50.0 100.0 150.0] (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty :ticks 3 :ticks-params {:min-n 3}}))))
    (is (= {:ticks 2.5} (try (s/ticks (s/scale :linear {:domain [0 1] :ticks-method :pretty :ticks 2.5}))
                             (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (is (= [0.0 0.5 1.0] (s/ticks (s/scale :linear {:domain [0 1] :ticks-method :pretty :ticks 2.0}))))
    (is (= {:ticks -1} (try (s/ticks (s/scale :log {:domain [1 1000] :ticks-method :pretty :ticks -1}))
                            (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (is (= {:min-n 7 :ticks 5}
           (try (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty :ticks-params {:min-n 7}}))
                (catch clojure.lang.ExceptionInfo e (ex-data e))))))
  (testing "explicit ticks win"
    (is (= [1 2] (s/ticks (s/scale :linear {:domain [0 150] :ticks-method :pretty :ticks [1 2]})))))
  (testing "pow, symlog, interpolated, log, datetime"
    (is (= [0.0 20.0 40.0 60.0 80.0 100.0] (s/ticks (s/scale :pow {:domain [0 100] :ticks-method :pretty}))))
    (is (= [-1000.0 -500.0 0.0 500.0 1000.0] (s/ticks (s/scale :symlog {:domain [-1000 1000] :ticks-method :pretty}))))
    (is (= [0.0 2.0 4.0 6.0 8.0 10.0] (s/ticks (s/scale :interpolated {:domain [0 3 10] :range [0 0.5 1] :ticks-method :pretty}))))
    (is (= [6.0 7.0 8.0 9.0] (s/ticks (s/scale :log {:domain [6 9] :ticks-method :pretty}))))
    (is (= [(LocalDateTime/of 2012 7 1 0 0) (LocalDateTime/of 2012 10 1 0 0)]
           (s/ticks (s/scale :datetime {:domain [(LocalDateTime/of 2012 5 5 10 10 10) (LocalDateTime/of 2012 12 5 10 10 10)]
                                        :ticks-method :pretty}))))))

(deftest unknown-ticks-method-test
  (doseq [sc [(s/scale :linear {:ticks-method :bogus})
              (s/scale :log {:ticks-method :bogus})
              (s/scale :datetime {:ticks-method :bogus})]]
    (is (= {:ticks-method :bogus} (try (s/ticks sc) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (is (thrown? clojure.lang.ExceptionInfo (s/nice sc)))))

(deftest discrete-kinds-ignore-method-test
  (doseq [[kind params] [[:bands {:domain [:a :b :c]}]
                         [:ordinal {:domain [:a :b] :range [1 2]}]
                         [:quantile {:domain [1 2 3 4 5 6 7 8]}]
                         [:quantize {:domain [0 10] :range 4}]
                         [:threshold {:domain [0.2 10 11] :range [1 2 3 4]}]
                         [:histogram {:domain [1 2 3 4 5 6 7 7 8 9]}]
                         [:constant {:range 1}]]
          method [:pretty :bogus]]
    (is (= (s/ticks (s/scale kind params)) (s/ticks (s/scale kind (assoc params :ticks-method method))))
        (str kind method))))
