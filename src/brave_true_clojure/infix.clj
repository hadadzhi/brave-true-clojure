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
      (if (number? token)
        (recur (rest tokens) stack (conj output token))
        (if (sequential? token)
          (recur (rest tokens) stack (into output (infix-to-rpn (seq token))))
          (if (operator? token)
            (let [last-op (first stack)]
              (if (and last-op (<= (precedence last-op) (precedence token)))
                (recur tokens (rest stack) (conj output (first stack)))
                (recur (rest tokens) (cons token stack) output)))
            (throw (IllegalArgumentException. (str token))))))
      (into output stack))))

(defn- rpn-to-prefix [rpn]
  (loop [tokens rpn, stack '()]
    (if-let [token (first tokens)]
      (if (number? token)
        (recur (rest tokens) (cons token stack))
        (if (operator? token)
          (let [left (second stack),
                right (first stack),
                stack (rest (rest stack))]
            (recur (rest tokens) (cons (list token left right) stack)))
          (throw (IllegalArgumentException. (str token)))))
      (if (= 1 (count stack))
        (first stack)
        (throw (IllegalArgumentException. "bad infix"))))))

(defmacro infix [& tokens]
  (rpn-to-prefix (infix-to-rpn tokens)))
