(ns infix)

(def ^:private operators "operator-symbol -> precedence"
  {'+ 1, '- 1, '* 0, '/ 0, 'quot 0, 'rem 0, 'mod 0})

(defn- operator? [token]
  (contains? operators token))

(defn- precedence [token]
  (operators token))

(defn- infix-to-rpn
  "Praise Dijkstra for this shunting-yard algorithm"
  [tokens]
  (loop [tokens tokens, stack (), output []]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [last-op (first stack)]
          (if (and last-op (<= (precedence last-op) (precedence token)))
            (recur tokens (rest stack) (conj output last-op))
            (recur (rest tokens) (conj stack token) output)))
        (recur (rest tokens) stack (if (vector? token)
                                     (into output (infix-to-rpn token))
                                     (conj output token))))
      (into output stack))))

(defn- rpn-to-prefix [rpn]
  (loop [tokens rpn, stack ()]
    (if-let [token (first tokens)]
      (if (operator? token)
        (let [left (second stack),
              right (first stack),
              stack (rest (rest stack))]
          (recur (rest tokens) (conj stack (list token left right))))
        (recur (rest tokens) (conj stack token)))
      (first stack))))

(defn- valid-infix? [tokens]
  (if (seq tokens)
    (loop [tokens tokens, prev nil]
      (if-let [curr (first tokens)]
        (let [next (first (rest tokens))]
          (if (operator? curr)
            (if (and prev next (not (operator? prev)) (not (operator? next)))
              (recur (rest tokens) curr))
            (if (vector? curr)
              (if (valid-infix? curr)
                (recur (rest tokens) curr))
              (if (or (and (not prev) (not next))
                      (and (not prev) (operator? next))
                      (and (not next) (operator? prev))
                      (and (operator? prev) (operator? next)))
                (recur (rest tokens) curr)))))
        true))))

(defmacro infix
  "Use [] for grouping, use () for arbitrary Clojure calls"
  [& tokens]
  (if (valid-infix? tokens)
    (rpn-to-prefix (infix-to-rpn tokens))
    (throw (IllegalArgumentException. "bad syntax"))))
