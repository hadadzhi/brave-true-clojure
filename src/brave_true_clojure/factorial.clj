(ns factorial
  (:require [clojure.core.reducers :refer [fold]]))

(def fac
  (fn [n]
    (if (< n 0)
      (throw (IllegalArgumentException.))
      (fold *' (vec (range 1 (inc n)))))))
