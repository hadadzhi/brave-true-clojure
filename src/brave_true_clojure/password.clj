(ns password
  (:import (java.security SecureRandom)))

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

(defn- char-range
  "Returns a sequence of chars from a to b inclusive."
  [a b] {:pre  [(char? a)
                (char? b)]
         :post [(seq? %)
                (every? char? %)]}
  (map char
       (range (int a)
              (inc (int b)))))

(def ^:private predefined-char-sets
  {:small   (char-range \a \z)
   :capital (char-range \A \Z)
   :digits  (char-range \0 \9)})

(defn- create-char-set
  "Returns a vector of characters specified by char-sets."
  [char-sets] {:pre  [(seq char-sets)
                      (every? #(or (predefined-char-sets %)
                                   (string? %)
                                   (and (seq? %) (every? char? %)))
                              char-sets)]
               :post [(vector? %)
                      (every? char %)]}
  (vec (mapcat #(if-let [char-set (predefined-char-sets %)]
                  char-set
                  (seq %))
               char-sets)))

(defn gen-password
  "Produces a random string of the specified length
   that's made of chars specified in char-sets.
   An element of char-sets may a be a sequence of characters
   or one of (:small :capital :digits)"
  [length & char-sets] {:pre  [(integer? length)
                               (> length 0)]
                        :post [(string? %)
                               (== length (count %))]}
  (let [char-set (create-char-set char-sets)
        char-set-length (count char-set)]
    (apply str
           (take length
                 (repeatedly #(char-set (secure-random-index char-set-length)))))))
