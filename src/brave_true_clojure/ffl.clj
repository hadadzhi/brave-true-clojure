(ns brave-true-clojure.ffl
  (:require [brave-true-clojure.factorial :refer :all]))

(defn cartesian
  "Returns the cartesian product of the arguments as a list of lists.
   Arguments must be seq-able."
  [& sets]
  (if (empty? sets)
    '(())
    (for [x    (first sets)
          more (apply cartesian (rest sets))]
      (cons x more))))

(defn combs
  "Returns all possible combinations of elems
   with length less than or equal to max-len."
  [elems max-len]
  (for [l (range 1 (inc max-len))]
    (apply cartesian (repeat l (set elems)))))

(defn roman-nums
  [max-len]
  (flatten
    (map (partial map (partial apply str))
         (combs "IVXLCDM" max-len))))

(defn flatten- [coll]
  (if (sequential? coll)
    (lazy-seq
      (reduce #(if (sequential? %2)
                 (into %1 (flatten- %2))
                 (conj %1 %2))
              [] coll))
    (lazy-seq)))

(defn char-range
  "Returns a sequence of chars from a to b inclusive."
  [a b] {:pre  [(char? a)
                (char? b)]
         :post [(seq? %)
                (every? char? %)]}
  (map char
       (range (int a)
              (inc (int b)))))

(defn permutations
  "Computes all permutations of a set.
   Returns a sequence of sequences."
  [s] {:pre  [(set? s)]
       :post [(or (and (empty? %) (empty? s))
                  (and (== (count %) (fac (count s)))
                       (every? #(= (set %) s) %)
                       (apply distinct? %)))]}
  (when (seq s)
    (if (seq (rest s))
      (for [e s, p (permutations (disj s e))]
        (cons e (flatten p)))
      (list (list (first s))))))

(defn clamp
  "Clamps the value between min and max inclusive."
  [value min max] {:pre  [(every? number? [value min max])
                          (<= min max)]
                   :post [(number? %)
                          (<= min % max)]}
  (cond (<= value min) min
        (>= value max) max
        :else value))

(defn ins
  "Inserts e at index i into coll v. Returns a vector.
   If i < 0, e is inserted at the head.
   If i > (count v), e is inserted at the tail."
  [e i v] {:pre  [(integer? i)
                  (coll? v)]
           :post [(let [i (clamp i 0 (count v))]
                    (and (= e (% i))
                         (= (subvec v 0 i) (subvec % 0 i))
                         (= (subvec % (inc i)) (subvec v i))))]}
  (let [[l r] (split-at i v)]
    (vec (concat l `(~e) r))))
