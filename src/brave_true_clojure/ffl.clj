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
  "Returns all possible combinations with length
   less than or equal to max-len of elems."
  [elems max-len]
  (for [l (range 1 (inc max-len))]
    (apply cartesian (repeat l (set elems)))))

(defn roman-nums
  [max-len]
  (flatten
    (map (partial map (partial apply str))
         (ffl/combs "IVXLCDM" max-len))))
