(ns brave-true-clojure.fclj
  (:import (clojure.lang ArityException))
  (:require [brave-true-clojure.ffl :refer :all]))

(defn fib-seq []
  ((fn fib-internal [curr next]
     (lazy-seq (cons curr (fib-internal next (+ curr next))))) 1N 1N))

;; #43 Reverse interleave
(defn reverse-interleave-1 [coll n]
  (letfn [(keep-nth [coll n]
            (keep-indexed #(if (= (mod (inc %1) n) 0) %2) coll))
          (reverse-interleave-internal [coll shift]
            (when (>= shift 0)
              (cons (keep-nth (if (> shift 0)
                                (concat (repeat shift nil)
                                        coll)
                                coll)
                              n)
                    (reverse-interleave-internal coll (dec shift)))))]
    (reverse-interleave-internal coll (dec n))))

;; simpler :)
(defn reverse-interleave [coll n]
  (apply map list (partition n coll)))

; This is simpler :)
(defn reverse-interleave-simpler [coll n]
  (apply map list (partition n coll)))

;; #44 Rotate collection
(defn rotate [n coll]
  (let [size (count coll)
        skip (if (> n 0) n (+ n (* size (- n))))]
    (take size (drop skip (cycle coll)))))

;; #46 Flipping out
(defn flip-args [fn]
  (letfn [(flipped [& args] (apply fn (reverse args)))]
    flipped))

;; #53 Longest subsequence
(defn longest-subseq [coll]
  (letfn [(find-asc-seqs [coll]
            (letfn [(asc-seq [coll]
                      (lazy-seq
                        (when-let [s (seq coll)]
                          (let [x (first s), y (second s)]
                            (if (and x y (> y x))
                              (cons x (asc-seq (rest s)))
                              (list x))))))]
              (if (empty? (rest coll))
                (list (asc-seq coll))
                (cons (asc-seq coll)
                      (find-asc-seqs (rest coll))))))]
    (let [seqs (filter #(> (count %) 1)
                       (reverse (find-asc-seqs coll)))]
      (if (seq seqs)
        (apply max-key count seqs)
        ()))))

;; #55 Count occurences
(defn wc [coll]
  (reduce (fn [m w] (assoc m w (inc (m w 0)))) {} coll))

;; #60 Reductions
(defn reductions-
  ([f coll]
   (lazy-seq
     (if-let [s (seq coll)]
       (reductions- f (first s) (rest s))
       (list (f)))))
  ([f init coll]
   (lazy-seq
     (if-let [s (seq coll)]
       (cons init (reductions- f (f init (first s)) (rest s)))
       (list init)))))

;; #65 Black box testing
(defn which [coll]
  (let [coll (conj (empty coll) [:a :b] [:a :b] [:c :d])]
    (if (= 2 (count coll))
      (if (coll [:a :b]) :set :map)
      (if (= [:a :b] (first coll)) :vector :list))))

;; #67 Prime numbers
(defn first-n-primes [n]
  (letfn [(factor? [n x]
            (zero? (rem n x)))
          (divisors [n]
            (conj (set (filter #(factor? n %)
                               (range 2 (inc (Math/sqrt n)))))
                  1 n))
          (prime? [n]
            (= 2 (count (divisors n))))]
    (take n (filter #(prime? %) (iterate inc 1)))))

;; #73 Крестики-нолики
;; The board is a vector of vectors, like this:
;; [[:e :e :x]
;;  [:e :x :e]
;;  [:x :e :e]]
(defn square-matrix? [m]
  (and (vector? m)
       (every? vector? m)
       (let [d (count m)]
         (every? #(= (count %) d) m))))

(defn transpose [m]
  (vec (apply map vector m)))

(defn major-diagonal [m] {:pre [(square-matrix? m)]}
  (vec (map-indexed #(%2 %1) m)))

(defn minor-diagonal [m] {:pre [(square-matrix? m)]}
  (major-diagonal (vec (reverse (apply map vector m)))))

(defn won-by? [player board]
  (let [row [player player player]]
    (or (some #{row} board)
        (some #{row} (transpose board))
        (= row (major-diagonal board))
        (= row (minor-diagonal board)))))

(defn winner [board]
  (cond (won-by? :x board) :x
        (won-by? :o board) :o))

;; #75 Euler's Totient function
(defn euler-totient [x]
  (cond
    (< x 0) (throw (IllegalArgumentException. "x < 0"))
    (= x 1) 1
    :else (letfn [(gcd [a b]
                    (if (= 0 b) a (recur b (rem a b))))
                  (coprime? [a b]
                    (= 1 (gcd a b)))]
            (count (filter #(coprime? % x) (range 1 x))))))

;; #77 Anagram finder
(defn find-anagrams
  "Returns a set of sets of anagrams in the input collection"
  [coll]
  (set
    (->> (group-by sort coll)
         (vals)
         (map set)
         (filter #(> (count %) 1)))))

;; #79 Triangle minimal path
(defn sum-min-path [triangle]
  ((fn walk-triangle [tri, n, sum]
     (if (= 1 (count (first tri)))
       (walk-triangle (rest tri) 0 ((first tri) 0N))
       (loop [s (seq tri), sum sum, n n]
         (if-let [row (first s)]
           (let [li n, ri (inc n), l (row li), r (row ri)]
             (if (< (walk-triangle (next s) li l)
                    (walk-triangle (next s) ri r))
               (recur (next s) (+ sum l) li)
               (recur (next s) (+ sum r) ri)))
           sum)))) triangle 0N 0N))

(defn make-triangle [n m]
  (let [m (inc m)]
    (loop [i 1, res []]
      (if (<= i n)
        (recur
          (inc i)
          (conj res (vec (take i (repeatedly (partial rand-int m))))))
        res))))

;; #80 Perfect numbers
(defn perfect? [n]
  (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n)))))

;; #108 Lazy search
(defn first-common
  "Returns the first common element in the given
  possibly infinite, sorted in increasing order sequences"
  [& seqs]
  (let [heads (map first seqs), max-head (apply max heads)]
    (if (apply = heads)
      max-head
      (recur (map (fn [seq]
                    (drop-while (fn [e]
                                  (< e max-head))
                                seq))
                  seqs)))))

;; #99 Digits
(defn digits [n]
  (cond (< n 0) (recur (- n))
        (< n 10) (conj [] n)
        :else (conj (digits (quot n 10)) (rem n 10))))

(defn digit-set
  "Works with any number."
  [x]
  (set (map read-string (re-seq #"\d" (str (num x))))))

;; 97 Pascal triangle
(defn pascal-row "1-based n" [n]
  (letfn [(elem [n k] (if (= 0 k)
                        1
                        (* (elem n (dec k)) (/ (- (inc n) k) k))))]
    (let [n (dec n), elem (partial elem n)]
      (vec (map elem (range 0 (inc n)))))))

;; Cards
(defn card [card-str]
  (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
        ranks {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7,
               \T 8, \J 9, \Q 10, \K 11, \A 12}]
    (if (= 2 (count card-str))
      {:suit (suits (first card-str)),
       :rank (ranks (second card-str))})))

;; LCM
(defn lcm
  ([a] a)
  ([a b]
   (letfn [(gcd [a b] (if (= 0 b) a (recur b (rem a b))))
           (abs [x] (if (< x 0) (- x) x))]
     (if (= 0 a b) 0 (/ (abs (* a b)) (gcd a b)))))
  ([a b & args]
   (apply lcm (lcm a b) args)))

;; 96 Beauty is Symmetry
(defn mirror? [l r]
  (or (= nil l r)
      (and (= (first l) (first r))
           (mirror? (second l) (last r))
           (mirror? (second r) (last l)))))

(defn symmetric? [tree]
  (mirror? (first tree) (last tree)))

;; 98 Equivalence classes
(defn eq-classes [function domain]
  (set (map set (vals (group-by function domain)))))

;; 101 Levenshtein distance
(defn levend [a b]
  (let [d (fn [self i j]
            (let [i-1 (dec i)
                  j-1 (dec j)
                  c (if (not= (get a i-1) (get b j-1)) 1 0)]
              (if (= 0 (min i j))
                (max i j)
                (min (+ 1 (self self i-1 j))
                     (+ 1 (self self i j-1))
                     (+ c (self self i-1 j-1))))))]
    (d (memoize d) (count a) (count b))))

;; 92 Read Roman numerals
(def r->d {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
(defn roman-to-decimal [roman-str]
  (loop [str roman-str, prev 0, output 0]
    (if-let [curr (r->d (first str))]
      (if (< prev curr)
        (recur (rest str) curr (+ (- output prev) (- curr prev)))
        (recur (rest str) curr (+ output curr)))
      output)))

;; 104 Write Roman numerals
(def d->r [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
           [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
           [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])
(defn decimal-to-roman [n]
  (cond (zero? n) ""
        (< n 0) (str "-" (decimal-to-roman (- n)))
        :else (let [sb (StringBuilder.), v (atom n)]
                (doseq [[d r] d->r]
                  (while (>= @v d)
                    (swap! v - d)
                    (.append sb r)))
                (str sb))))

;; 86 Happy numbers
(defn happy? [number]
  (cond (== 1 number) true
        (== 4 number) false
        :else (recur (->> (str number)
                          (map #(- (int %) 48))
                          (map #(* % %))
                          (reduce +)))))

;; 146 Trees into tables
(defn mflatten [m]
  (into {}
        (for [[k im] m,
              [ik v] im]
          [[k ik] v])))

;; 153 Pairwise Disjoint Sets
(defn disjoint? [sets]
  (== (count (reduce into #{} sets))
      (reduce + (map count sets))))

(defn disjoint?-slower [sets]
  (apply distinct? (apply concat sets)))

;; 158 Decurry
(defn decurry-iter
  "Uglier and slower!"
  [f]
  (fn [& args]
    (let [throw-arity #(throw (ArityException. (count args) (str f)))]
      ((fn [ifunc iargs]
         (if-let [first-arg (first iargs)]
           (let [ret (ifunc first-arg)]
             (if (fn? ret)
               (if-let [rest-args (rest iargs)]
                 (recur ret rest-args)
                 (throw-arity))
               (if (empty? (rest iargs))
                 ret
                 (throw-arity))))
           (throw-arity)))
        f args))))

(defn decurry-reduce
  "Prettier and faster!"
  [f]
  (fn [& args]
    (let [throw-arity #(throw (ArityException. (count args)
                                               (str f)))
          result (reduce #(if (fn? %1)
                            (%1 %2)
                            (throw-arity))
                         f
                         args)]
      (if (fn? result)
        (throw-arity)
        result))))

;; 132 Insert between
(defn insert-between
  "Returns a seq of elements of coll with item inserted
   between each two consecutive elements that satisfy the
   two-argument predicate pred?"
  [item pred? [head & tail :as coll]]
  (if head
    (cons head
          (mapcat #(if (pred? %1 %2) [item %2] [%2])
                  coll
                  tail))))

;; 147 Pascal's Trapezoid
(defn pascal-trapezoid [v]
  (iterate #(vec (map + (cons 0 %) (conj % 0))) v))

;; 115 The Balance of N
(defn balanced? [n]
  (let [sn (str n)
        hl (int (/ (count sn) 2))
        sum (fn [chars] (->> chars
                             (map #(- (int %) 48))
                             (reduce +)))]
    (== (sum (take hl sn))
        (sum (take hl (reverse sn))))))

;; 85 Power set
(defn subsets [s]
  (if (seq s)
    (let [h (first s)
          srs (subsets (rest s))]
      (set (concat srs
                   (map #(conj % h)
                        srs))))
    #{#{}}))

;; 105 Identify keys and values
(defn v->m [v]
  (loop [v v, k nil, m {}]
    (let [e (first v)]
      (if (keyword? e)
        (recur (rest v) e (assoc m e []))
        (if (number? e)
          (recur (rest v) k (assoc m k (conj (m k) e)))
          m)))))

;; 137 Digits and bases
(defn digit-seq [n b] {:pre [(>= n 0) (> b 1)]}
  (if (zero? n)
    [0]
    (let [s (conj (digit-seq (quot n b) b)
                  (rem n b))]
      (if (zero? (first s))
        (subvec s 1)
        s))))

;; 144 Oscilrate
(defn oscilrate-1 [e & fs]
  (lazy-seq
    (cons e
          (apply oscilrate-1
                 ((first fs) e)
                 (rest (cycle fs))))))

;; 1.7x slower
(defn oscilrate-2 [e & fs]
  (reductions #(%2 %1)
              e
              (cycle fs)))

;; 110 Sequence of pronunciations
(defn pronunciations [digit-seq]
  (letfn [(pronunciation [digit-seq]
            (loop [in (rest digit-seq), out [1], prev (first digit-seq)]
              (if (seq in)
                (if (== prev (first in))
                  (recur (rest in)
                         (assoc out
                           (dec (count out))
                           (inc (out (dec (count out)))))
                         prev)
                  (recur in
                         (conj out prev 0)
                         (first in)))
                (conj out prev))))]
    (let [p (pronunciation digit-seq)]
      (lazy-seq
        (cons p (pronunciations p))))))

;; slower
(defn pron [s]
  (mapcat (juxt count first)
          (partition-by identity s)))

(defn prons [s]
  (rest (iterate pron s)))

;; 113 Global take-while
(defn take-up-to-nth
  "Returns a lazy sequence of items in s up to, but not including,
   the nth item that satisfies the predicate."
  [n pred? s]
  (lazy-seq
    (let [lr (split-with (complement pred?) s)]
      (if (> n 1)
        (concat (first lr)
                (cons (first (second lr))
                      (take-up-to-nth (dec n) pred? (rest (second lr)))))
        (first lr)))))

;; 103 Generating k-combinations
(defn kcombinations [k s]
  (letfn [(cartesian-seq
            ([s1 s2]
             (for [a (set s1), b (set s2)]
               (list a b)))
            ([s1 s2 & ss]
             (for [a (set s1), b (apply cartesian-seq s2 ss)]
               (cons a b))))]
    (cond (or (= 0 k) (> k (count s))) #{}
          (= 1 k) (set (map hash-set s))
          (= (count s) k) (set s)
          :else (->> (apply cartesian-seq (repeat k s))
                     (map set)
                     (distinct)
                     (filter #(= k (count %)))
                     (set)))))

(defn ncombinations [k n]
  (/ (factorial n)
     (* (factorial k)
        (factorial (- n k)))))

;; 116 Balanced prime
(defn balanced-prime? [n]
  (letfn [(prime? [n]
            (and (> n 1)
                 (every? #(not= 0 (mod n %)) (range 2 n))))
          (find-lower [n]
            (when (> n 2)
              (first (filter prime? (iterate dec (dec n))))))
          (find-upper [n]
            (when (> n 2)
              (first (filter prime? (iterate inc (inc n))))))]
    (true?
      (and (prime? n)
           (when-let [l (find-lower n)]
             (when-let [u (find-upper n)]
               (= n (/ (+ l u) 2))))))))

;; 121 Universal computation engine
(defn compile-expr [expr]
  (letfn [(self [expr vars]
            (let [op {'+ +, '- -, '* *, '/ /}]
              (if (list? expr)
                (apply (op (first expr)) (map #(self % vars) (rest expr)))
                (if (symbol? expr)
                  (vars expr)
                  expr))))]
    (fn [vars]
      (self expr vars))))
