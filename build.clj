(ns build
  "Wadogo build script.

  clojure -T:build test     ; run tests
  clojure -T:build jar      ; build the jar (target/)
  clojure -T:build install  ; install the jar into the local Maven repository
  clojure -T:build deploy   ; deploy the jar to Clojars"
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.scicloj/wadogo)
(def version "1.1.0-alpha2-SNAPSHOT")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn test "Run all the tests." [opts]
  (let [basis (b/create-basis {:aliases [:test]})
        cmd (b/java-command {:basis basis
                             :main 'clojure.main
                             :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmd)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {:exit exit}))))
  opts)

(def ^:private pom-data
  [[:description "Scales for Clojure"]
   [:url "https://github.com/scicloj/wadogo"]
   [:licenses
    [:license
     [:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"]
     [:url "https://www.eclipse.org/legal/epl-2.0/"]]]
   [:scm
    [:url "https://github.com/scicloj/wadogo"]
    [:connection "scm:git:https://github.com/scicloj/wadogo.git"]
    [:developerConnection "scm:git:ssh:git@github.com:scicloj/wadogo.git"]
    [:tag (str "v" version)]]])

(defn jar "Build the jar." [opts]
  (b/delete {:path "target"})
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis (b/create-basis {})
                :src-dirs ["src"]
                :pom-data pom-data})
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  (b/jar {:class-dir class-dir :jar-file jar-file})
  opts)

(defn install "Install the jar into the local Maven repository." [opts]
  (jar opts)
  (b/install {:basis (b/create-basis {})
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir})
  opts)

(defn deploy "Deploy the jar to Clojars (CLOJARS_USERNAME and CLOJARS_PASSWORD environment variables)." [opts]
  (jar opts)
  (dd/deploy {:installer :remote
              :artifact (b/resolve-path jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})})
  opts)
