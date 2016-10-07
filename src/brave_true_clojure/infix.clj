(ns brave-true-clojure.infix)

(def ^:private operators "operator-symbol -> precedence"
  {'+ 1, '- 1, '* 0, '/ 0, 'quot 0, 'rem 0, 'mod 0})

(defn- operator? [token]
  (contains? operators token))

(defn- precedence [token]
  (operators token))

(defn- bad-syntax []
  (throw (IllegalArgumentException. "bad syntax")))

(defn- infix-to-rpn
  "Praise Dijkstra for this shunting yard algorithm"
  [tokens]
  (loop [tokens tokens, stack '(), output []]
    (if-let [token (first tokens)]
      (if (and (sequential? token) (not (symbol? (first token))))
        (recur (rest tokens) stack (into output (infix-to-rpn token)))
        (if (operator? token)
          (let [last-op (first stack)]
            (if (and last-op (<= (precedence last-op) (precedence token)))
              (recur tokens (rest stack) (conj output (first stack)))
              (recur (rest tokens) (cons token stack) output)))
          (recur (rest tokens) stack (conj output token))))
      (into output stack))))

(defn- rpn-to-prefix [rpn]
  (loop [tokens rpn, stack '()]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [left (second stack),
              right (first stack),
              stack (rest (rest stack))]
          (if (and left right)
            (recur (rest tokens) (cons (list token left right) stack))
            (bad-syntax)))
        (recur (rest tokens) (cons token stack)))
      (if (= 1 (count stack))
        (first stack)
        (bad-syntax)))))

(defmacro infix [& tokens]
  (rpn-to-prefix (infix-to-rpn tokens)))
