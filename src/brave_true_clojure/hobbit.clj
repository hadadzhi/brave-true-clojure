(ns brave-true-clojure.hobbit)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 9}
                             {:name "back" :size 9}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn right-part [part]
  {:name (clojure.string/replace (part :name) #"^left-" "right-")
   :size (part :size)})

(defn symmetrize-parts [parts]
  (reduce #(into %1 (set [%2 (right-part %2)])) [] parts))

(defn hit [asym-body-parts]
  (let [body-parts (symmetrize-parts asym-body-parts)
        body-size (reduce + (map :size body-parts))
        target (rand body-size)
        detect-hit (fn [prev-acc-size part]
                     (let [acc-size (+ prev-acc-size (part :size))]
                       (if (> acc-size target) (reduced part) acc-size)))]
    (reduce detect-hit 0 body-parts)))
