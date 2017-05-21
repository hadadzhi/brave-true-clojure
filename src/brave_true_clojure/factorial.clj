(ns brave-true-clojure.factorial
  (:require [clojure.core.reducers :refer [fold]]))

(defn fac [n] {:pre  [(>= n 0)]
               :post [(integer? %)]}
  (fold *' (vec (range 1 (inc n)))))
