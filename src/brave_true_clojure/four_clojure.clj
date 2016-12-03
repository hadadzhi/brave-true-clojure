(ns brave-true-clojure.four-clojure)

(defn fac [n]
 (if (< n 0)
  (throw (new IllegalArgumentException))
  (reduce *' (range 1 (inc n)))))

(defn fib-seq []
 ((fn fib-internal [curr next]
   (lazy-seq (cons curr (fib-internal next (+ curr next))))) 1N 1N))

;; #43 Reverse interleave
(defn reverse-interleave [coll n]
 (letfn [(keep-nth [coll n]
          (keep-indexed #(if (= (mod (inc %1) n) 0) %2) coll))
         (reverse-interleave-internal [coll shift]
          (when (>= shift 0)
           (cons
            (keep-nth
             (if (> shift 0) (concat (repeat shift nil) coll) coll) n)
            (reverse-interleave-internal coll (dec shift)))))]
  (reverse-interleave-internal coll (dec n))))

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
           (if (empty? (rest coll)) (list (asc-seq coll))
                                    (cons (asc-seq coll) (find-asc-seqs (rest coll))))))]
  (let [seqs (filter #(> (count %) 1) (reverse (find-asc-seqs coll)))]
   (if (seq seqs) (apply max-key count seqs) ()))))

;; #55 Count occurences
(defn wc [coll]
 (reduce (fn [m w] (assoc m w (inc (m w 0)))) {} coll))

;; #60 Reductions
(defn toy-reductions
 ([f coll]
  (lazy-seq
   (if-let [s (seq coll)]
    (toy-reductions f (first s) (rest s))
    (list (f)))))
 ([f init coll]
  (lazy-seq
   (if-let [s (seq coll)]
    (cons init (toy-reductions f (f init (first s)) (rest s)))
    (list (f init))))))

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
          (conj (set (filter #(factor? n %) (range 2 (inc (Math/sqrt n))))) 1 n))
         (prime? [n]
          (= 2 (count (divisors n))))]
  (take n (filter #(prime? %) (iterate inc 1)))))

;; #73 Крестики-нолики
(defn transpose [m]
 (apply map vector m))

(defn major-diagonal [m]
 (loop [res [], n 0]
  (let [row (m n)]
   (if (< n (dec (count row)))
    (recur (conj res (row n)) (inc n))
    (conj res (row n))))))

(defn minor-diagonal [m]
 (loop [res [], n 0]
  (let [row (vec (reverse (m n)))]
   (if (< n (dec (count row)))
    (recur (conj res (row n)) (inc n))
    (conj res (row n))))))

(defn won-by [player board]
 (let [row [player player player]]
  (boolean
   (or
    (some #{row} board)
    (some #{row} (transpose board))
    (= row (major-diagonal board))
    (= row (minor-diagonal board))))))

(defn who-won [board]
 (cond
  (won-by :x board) :x
  (won-by :o board) :o
  :else nil))

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
       (if (< (walk-triangle (next s) li l) (walk-triangle (next s) ri r))
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
   (recur (map (fn [seq] (drop-while (fn [e] (< e max-head)) seq)) seqs)))))

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
 (letfn [(elem [n k] (if (= 0 k) 1 (* (elem n (dec k)) (/ (- (inc n) k) k))))]
  (let [n (dec n), elem (partial elem n)]
   (vec (map elem (range 0 (inc n)))))))

;; Cards
(defn card [card-str]
 (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
       ranks {\2 0,
              \3 1,
              \4 2,
              \5 3,
              \6 4,
              \7 5,
              \8 6,
              \9 7,
              \T 8,
              \J 9,
              \Q 10,
              \K 11,
              \A 12}]
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
