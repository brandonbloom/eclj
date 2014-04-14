(defproject eclj "0.1.0-SNAPSHOT"
  :description "Extensible Clojure"
  :url "https://github.com/brandonbloom/eclj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.4"]]
  :profiles
  {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.4-SNAPSHOT"]
                        [fipp "0.4.1"]]}})
