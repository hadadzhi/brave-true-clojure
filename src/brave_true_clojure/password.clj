(ns brave-true-clojure.password
  (:import (java.security SecureRandom))
  (:require [brave-true-clojure.ffl :refer [char-range]]))

(def ^:private secure-random (SecureRandom.))

(defn- secure-random-index
  "Produces an integer in range [0; upper-bound)
   using a cryptographically strong RNG."
  [upper-bound] {:pre  [(integer? upper-bound)
                        (> upper-bound 0)]
                 :post [(integer? %)
                        (>= % 0)
                        (< % upper-bound)]}
  (.nextInt secure-random upper-bound))

(def ^:private predefined-char-sets
  {:small   (char-range \a \z)
   :capital (char-range \A \Z)
   :digits  (char-range \0 \9)})

(defn- char-vec
  "Returns a vector of characters specified by char-sets."
  [char-sets] {:pre  [(seq char-sets)
                      (every? #(or (predefined-char-sets %)
                                   (string? %)
                                   (and (seqable? %)
                                        (every? char? %)))
                              char-sets)]
               :post [(vector? %)
                      (every? char %)]}
  (vec (reduce #(into %1
                      (or (predefined-char-sets %2)
                          (set (seq %2))))
               #{}
               char-sets)))

(defn gen-password
  "Produces a random string of the specified length
   made of chars specified by char-sets.
   An element of char-sets must be either a sequence of characters
   or one of the keywords (:small :capital :digits).
   The default length and character set is 32
   and small and capital letters and digits."
  ([length & char-sets] {:pre  [(integer? length)
                                (> length 0)]
                         :post [(string? %)
                                (== length (count %))]}
   (let [cs  (char-vec char-sets)
         csl (count cs)]
     (apply str (->> (repeatedly #(cs (secure-random-index csl)))
                     (take length)))))
  ([length] (gen-password length :small :capital :digits))
  ([] (gen-password 32)))
