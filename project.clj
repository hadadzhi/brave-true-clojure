(defproject brave-true-clojure "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [criterium "0.4.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
