(ns brave-true-clojure.typo-correction-test
  (:require [clojure.test :refer :all]
            [brave-true-clojure.ffl :refer [char-range
                                            norvig-typo-corrector]]))

(deftest test-typo-correction
  (let [f (->> (slurp "resources/big.txt")
               (.toLowerCase)
               (re-seq #"[a-z]+")
               (frequencies))
        a (char-range \a \z)
        c (norvig-typo-corrector f a)]
    (testing "Norvig's tests"
      (is (= "spelling" (c "speling")))
      (is (= "corrected" (c "korrectud")))
      (is (= "bicycle" (c "bycycle")))
      (is (= "inconvenient" (c "inconvient")))
      (is (= "arranged" (c "arrainged")))
      (is (= "poetry" (c "peotry")))
      (is (= "poetry" (c "peotryy")))
      (is (= "word" (c "word")))
      (is (= "quintessential" (c "quintessential"))))))
