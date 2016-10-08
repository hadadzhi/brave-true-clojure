(ns brave-true-clojure.infix)

(def ^:private operators "operator-symbol -> precedence"
  {'+ 1, '- 1, '* 0, '/ 0, 'quot 0, 'rem 0, 'mod 0})

(defn- operator? [token]
  (contains? operators token))

(defn- precedence [token]
  (operators token))

(defn- infix-to-rpn
  "Praise Dijkstra for this shunting yard algorithm"
  [tokens]
  (loop [tokens tokens, stack '(), output []]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [last-op (first stack)]
          (if (and last-op (<= (precedence last-op) (precedence token)))
            (recur tokens (rest stack) (conj output (first stack)))
            (recur (rest tokens) (conj stack token) output)))
        (if (vector? token)
          (recur (rest tokens) stack (into output (infix-to-rpn token)))
          (recur (rest tokens) stack (conj output token))))
      (into output stack))))

(defn- rpn-to-prefix [rpn]
  (loop [tokens rpn, stack '()]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [left (second stack),
              right (first stack),
              stack (rest (rest stack))]
          (recur (rest tokens) (conj stack (list token left right))))
        (recur (rest tokens) (conj stack token)))
      (first stack))))

(defn- valid-infix? [infix-tokens]
  (loop [tokens infix-tokens, last nil]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [next (first (rest tokens))]
          (if (and last next (not (operator? last)) (not (operator? next)))
            (recur (rest tokens) token)
            false))
        (recur (rest tokens) token))
      true)))

(defn- bad-syntax []
  (throw (IllegalArgumentException. "bad syntax")))

(defmacro infix
  "Use [] for grouping, use () for arbitrary Clojure calls"
  [& tokens]
  (if (valid-infix? tokens)
    (rpn-to-prefix (infix-to-rpn tokens))
    (bad-syntax)))
