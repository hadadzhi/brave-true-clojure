(ns brave-true-clojure.edn-converters
  (:import (java.time Instant)
           (java.io Writer)))

(defmethod print-method
  Instant
  [^Instant instant ^Writer writer]
  (print-simple "#brave-true-clojure/Instant" writer)
  (print-method [(.getEpochSecond instant)
                 (.getNano instant)]
                writer))

(defn read-instant
  [[secs nanos :as instant-vector]]
  (Instant/ofEpochSecond secs nanos))
