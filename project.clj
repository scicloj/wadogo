(defproject wadogo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  :profiles {:dev {:cloverage    {:runner :midje}
                   :dependencies [[midje "1.9.10"]
                                  [scicloj/notespace "3-beta4"]]
                   :plugins      [[lein-midje "3.2.2"]
                                  [lein-cloverage "1.2.2"]]}})
