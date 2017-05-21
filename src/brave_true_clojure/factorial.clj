(ns brave-true-clojure.factorial
  (:require [clojure.core.reducers :refer [fold]]))

(defn fac [n] {:pre  [(>= n 0)]
               :post [(integer? %)]}
  (cond (<= n 1) 1
        (<= n 20) (* n (fac (- n 1)))
        :else (fold *' (vec (range 1 (inc n))))))
