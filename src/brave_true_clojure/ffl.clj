(ns brave-true-clojure.ffl
  (:require [clojure.core.reducers :refer [fold]]))

(defn factorial
  "Computes the factorial the fancy way."
  [n] {:pre  [(>= n 0)]
       :post [(integer? %)]}
  (cond (<= n 1) 1
        (<= n 20) (* n (factorial (- n 1)))
        :else (fold *' (vec (range 1 (inc n))))))

(defn cartesian
  "Returns the cartesian product. Arguments must be seq-able."
  [s1 s2 & ss] {:pre [(every? seqable? (conj ss s1 s2))]}
  (letfn [(cartesian-seq
            ([s1 s2]
             (for [a (set s1), b (set s2)]
               (list a b)))
            ([s1 s2 & ss]
             (for [a (set s1), b (apply cartesian-seq s2 ss)]
               (cons a b))))]
    (set (apply cartesian-seq s1 s2 ss))))

(defn combinations
  "Returns all possible combinations of elems (seqable)
   of length less than or equal to max-len."
  [elems max-len] {:pre [(seqable? elems) (> max-len 0)]}
  (apply concat
         (map list elems)
         (for [l (range 2 (inc max-len))]
           (apply cartesian (repeat l elems)))))

(defn roman-nums [max-len]
  (map (partial apply str)
       (combinations "IVXLCDM" max-len)))

(defn flattenv
  "flatten, but not lazy and returns a vector."
  [coll] {:pre [(or (nil? coll) (sequential? coll))]}
  (reduce #(if (sequential? %2)
             (into %1 (flattenv %2))
             (conj %1 %2))
          [] coll))

(defn char-range
  "Returns a sequence of chars from a to b inclusive."
  [a b] {:pre  [(every? char? [a b])]
         :post [(seq? %) (every? char? %)]}
  (map char
       (range (int a)
              (inc (int b)))))

(defn permutations
  "Computes all permutations of a set.
   Returns a sequence of sequences."
  [s] {:pre  [(set? s)]
       :post [(or (and (empty? %) (empty? s))
                  (and (== (count %) (factorial (count s)))
                       (every? #(= (set %) s) %)
                       (apply distinct? %)))]}
  (when (seq s)
    (if (seq (rest s))
      (for [e s, p (permutations (disj s e))]
        (cons e (flattenv p)))
      (list (list (first s))))))

(defn clamp
  "Clamps value to [min; max]."
  [value min max] {:pre  [(every? number? [value min max])
                          (<= min max)]
                   :post [(number? %)
                          (<= min % max)]}
  (cond (< value min) min
        (> value max) max
        :else value))

(defn insert
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

;; clojure.core/frequencies is better :)
(defn frequencies-
  "Counts the frequencies of each distinct element in s."
  [s]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} s))

(defn edits
  "Returns a set of results of applying n simple edits to word, where n > 0."
  [n alphabet word] {:pre [(> n 0) (every? char? alphabet) (string? word)]}
  (set
    (if (> n 1)
      (mapcat (partial edits 1 alphabet) (edits (- n 1) alphabet word))
      (let [length (count word)
            deletions (for [i (range length)]
                        (str (subs word 0 i) (subs word (inc i))))
            insertions (for [i (range (inc length)), c alphabet]
                         (str (subs word 0 i) c (subs word i)))
            replacements (for [i (range length), c alphabet]
                           (str (subs word 0 i) c (subs word (inc i))))
            transpositions (for [i (range (dec length))]
                             (str (subs word 0 i)
                                  (nth word (inc i))
                                  (nth word i)
                                  (subs word (+ 2 i))))]
        (concat deletions insertions replacements transpositions)))))
(alter-var-root #'edits memoize)

(defn norvig-typo-corrector
  "Returns a fn that takes a word and returns the most probable correction.
   Parameters:
   wc - a map from words to their frequencies,
   alphabet - a sequence of all characters in the alphabet."
  [wc alphabet] {:pre [(every? char? alphabet)
                       (every? #(and (string? (first %))
                                     (integer? (second %))) wc)]}
  (memoize
    (fn [word]
      (apply max-key
             #(get wc % 1)
             (or (not-empty (filter wc [word]))
                 (not-empty (filter wc (edits 1 alphabet word)))
                 (not-empty (filter wc (edits 2 alphabet word)))
                 [word])))))

(defn wrand [slices]
  (let [total (reduce + slices), r (rand total)]
    (loop [i 0, sum 0]
      (let [sum (+ (slices i) sum)]
        (if (< r sum)
          i
          (recur (inc i) sum))))))

(letfn [(fizzbuzzify [x]
          (if (zero? x)
            x
            (if (zero? (rem x 15))
              "fizzbuzz"
              (if (zero? (rem x 3))
                "fizz"
                (if (zero? (rem x 5))
                  "buzz"
                  x)))))]
  (defn fizzbuzz [start]
    (lazy-seq
      (cons (fizzbuzzify start)
            (fizzbuzz (inc start))))))

(defn fizzbuzz-1 []
  (->> (range)
       (map (fn [x]
              [x (if (zero? (rem x 3)) "fizz")]))
       (map (fn [[x s]]
              [x (str s (if (zero? (rem x 5)) "buzz"))]))
       (map #(if (or (zero? (first %))
                     (empty? (second %)))
               (first %)
               (second %)))))
