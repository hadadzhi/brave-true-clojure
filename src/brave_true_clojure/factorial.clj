(ns brave-true-clojure.factorial
  (:require [clojure.core.reducers :refer [fold]]))

(defn fac [n] {:pre  [(>= n 0)]
               :post [(integer? %)]}
  (if (< n 21)
    (if (== 0 n) 1 (* n (fac (- n 1))))
    (fold *' (vec (range 1 (inc n))))))
