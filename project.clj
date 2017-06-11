(defproject brave-true-clojure "0.1.0-SNAPSHOT"
  :plugins [[lein-ubersource "0.1.1"]]
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [criterium "0.4.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
