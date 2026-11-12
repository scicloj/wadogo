(ns wadogo.scale.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.core :as m]
            [wadogo.scale :as s])
  (:import [java.time LocalDateTime Duration]))

(def dt1 (LocalDateTime/of 2012 5 5 10 10 10))
(def dt2 (LocalDateTime/of 2012 12 5 10 10 10))

(def samples
  "A scale of every kind and values of its domain."
  {:linear [{:domain [-1 2] :range [100 200]} [-1 0 0.5 2 3]]
   :identity [{:domain [0 10]} [-1 0 3 10]]
   :pow [{:domain [0 100] :exponent 2} [0 10 50 100]]
   :log [{:domain [1 1000] :range [0 3]} [1 10 100 1000]]
   :symlog [{:domain [-100 100]} [-100 -1 0 1 100]]
   :radial [{:domain [0 1] :range [0 10]} [0 0.25 1]]
   :interpolated [{:domain [0 1 2] :range [0 10 40] :interpolator :cubic} [0 0.5 1 2]]
   :sequential [{:domain [10 20] :transform :pow :exponent 2} [10 15 20]]
   :diverging [{:domain [-1 0 4]} [-1 0 2 4]]
   :datetime [{:domain [dt1 dt2] :range [0 100]} [dt1 dt2]]
   :quantize [{:domain [0 10] :range [:a :b :c]} [0 4 9 10]]
   :quantile [{:domain [3 6 7 8 8 10 13 15 16 20] :range [0 1 2 3]} [3 8 9 20]]
   :threshold [{:domain [0 1] :range [:a :b :c]} [-1 0 0.5 1 2]]
   :histogram [{:domain [1 2 3 4 5 6 7 7 8 9] :range 3} [1 5 9]]
   :bands [{:domain [:a :b :c] :range [0 100] :padding-in 0.1} [:a :b :c]]
   :point [{:domain [:a :b :c] :range [0 100] :padding 0.5} [:a :b :c]]
   :ordinal [{:domain [:a :b :c] :range [1 2]} [:a :b :c]]
   :constant [{:domain [1 2] :range 5} [1 2 3]]})

(deftest scale-kinds-test
  (is (= 18 (count s/scale-kinds)))
  (is (= (set s/scale-kinds) (set (keys samples))))
  (is (= (set s/scale-kinds) (set (keys s/mapping)))))

(deftest kind-info-test
  (doseq [k s/scale-kinds]
    (is (and (string? (s/kind-info k)) (> (count (s/kind-info k)) 300)) (str k)))
  (is (= {:kind :foo} (try (s/kind-info :foo) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (is (= {:kind nil} (try (s/kind-info nil) (catch clojure.lang.ExceptionInfo e (ex-data e))))))

(deftest defaults-test
  (testing "every kind except quantile and histogram can be built without attributes"
    (doseq [k (remove #{:quantile :histogram} s/scale-kinds)]
      (is (s/scale? (s/scale k)) (str k))))
  (testing "quantile and histogram need a domain"
    (doseq [k [:quantile :histogram]]
      (is (re-find #"Domain can't be empty"
                   (try (s/scale k) (catch AssertionError e (ex-message e))))
          (str k)))))

(deftest apply-test
  (let [l (s/scale :linear {:domain [0 1] :range [0 10]})
        t (s/scale :threshold {:domain [0] :range [:a :b]})]
    (is (= 5.0 (apply l [0.5])))
    (is (= {:dstart 0 :dend ##Inf :id 1 :value :b} (apply t [1 true])))
    (is (= [0.0 10.0] (map l [0 1])))
    (is (thrown? clojure.lang.ArityException (apply t [1 2 3])))
    (is (thrown? clojure.lang.ArityException (apply t [])))))

(defn- forward-values [sc xs] (map #(sc %) xs))

(deftest round-trip-test
  (doseq [[k [attrs xs]] samples
          :let [sc (s/scale k attrs)
                expected (forward-values sc xs)]]
    (testing (str k)
      (is (= expected (forward-values (s/scale k (s/->map sc)) xs)) "->map")
      (is (= expected (forward-values (s/with-domain sc (s/domain sc)) xs)) "with-domain")
      (is (= expected (forward-values (s/update-domain sc identity) xs)) "update-domain")
      (is (= expected (forward-values (s/with-data sc {}) xs)) "with-data")
      (is (= expected (forward-values (s/with-ticks sc (s/ticks sc)) xs)) "with-ticks")
      (is (= expected (forward-values (s/with-formatter sc nil) xs)) "with-formatter")
      (is (= expected (forward-values (s/with-kind sc k) xs)) "with-kind")
      (is (= (seq (s/ticks sc)) (seq (s/ticks (s/scale k (s/->map sc))))) "ticks")
      (is (= (s/format sc) (s/format (s/scale k (s/->map sc)))) "format")
      (when-not (#{:identity :sequential :diverging :quantile :histogram} k)
        (is (= expected (forward-values (s/with-range sc (s/range sc)) xs)) "with-range")
        (is (= expected (forward-values (s/update-range sc identity) xs)) "update-range")))))

(deftest with-functions-test
  (let [l (s/scale :linear)]
    (is (= [2 3] (s/domain (s/with-domain l [2 3]))))
    (is (= [4.0 6.0] (s/domain (s/update-domain (s/with-domain l [2 3]) #(map (partial * 2.0) %)))))
    (is (= [5 6] (s/range (s/with-range l [5 6]))))
    (is (= 7 (s/data (s/with-data l :a 7) :a)))
    (is (= {:a 7 :b 8} (select-keys (s/data (s/with-data l {:a 7 :b 8})) [:a :b])))
    (is (= 3 (count (s/ticks (s/with-ticks l [1 2 3])))))
    (is (= ["x" "x"] (s/format (s/with-formatter l (constantly "x")) [1 2])))
    (is (= :log (s/kind (s/with-kind (s/scale :linear {:domain [1 10]}) :log))))))

(deftest size-test
  (is (= 100.0 (s/size (s/scale :linear {:range [200 100]}))))
  (is (= 3.0 (s/size (s/scale :linear {:domain [-1 2]}) :domain)))
  (is (= 3 (s/size (s/scale :bands {:domain [:a :b :c]}) :domain)))
  (is (= 1 (s/size (s/scale :bands) :domain)))
  (is (= 3 (s/size (s/scale :quantize {:range [1 2 3]}))))
  (is (= (Duration/ofHours 5136) (s/size (s/scale :datetime {:domain [dt1 dt2]}) :domain)))
  (is (= (Duration/ofHours 5136) (s/size (s/scale :datetime {:domain [dt2 dt1]}) :domain))))

(deftest extent-test
  (is (= [-1.0 5.0] (s/extent [3 -1 5 0])))
  (is (= [2.0 2.0] (s/extent [2]))))

(s/defcustom :test-sqrt m/sqrt m/sq)

(deftest defcustom-test
  (let [sc (s/scale :test-sqrt {:domain [0 100] :range [0 10]})]
    (is (= :test-sqrt (s/kind sc)))
    (is (m/delta-eq 5.0 (sc 25)))
    (is (m/delta-eq 25.0 (s/inverse sc 5)))
    (is (= 10.0 ((s/with-data sc :clamp true) 400)))
    (is (= :u ((s/with-data sc :unknown :u) nil)))
    (is (= [0.0 20.0 40.0 60.0 80.0 100.0] (s/ticks (s/with-data sc :ticks-method :pretty))))
    (is (string? (s/kind-info :test-sqrt)))
    (is (nil? (s/size sc)))))
