(ns ffl)

(defn cartesian
  "Returns the cartesian product of the arguments as a list of lists.
   Arguments must be seq-able."
  [& sets]
  (if (empty? sets)
    '(())
    (for [x (first sets)
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
         (ffl/combs "IVXLCDM" max-len))))

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
