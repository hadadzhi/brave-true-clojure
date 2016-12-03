(ns brave-true-clojure.infix-test
 (:require [clojure.test :refer :all]
           [brave-true-clojure.infix :refer :all]))

(deftest calculations
 (testing "Operators and precedence"
  (is (= 31/5 (infix 1 + 2 * 3 - 4 / 5)))
  (is (= (+ 1 (mod 2 -3)) (infix 1 + 2 mod -3)))
  (is (= (+ 1 (rem 2 -3)) (infix 1 + 2 rem -3)))
  (is (= (+ 1 (quot 2 -3)) (infix 1 + 2 quot -3))))
 (testing "Grouping"
  (is (= -3/5 (infix [1 + 2] * [3 - 4] / 5))))
 (testing "Function calls"
  (is (= (Math/sin (/ Math/PI 2))
         (infix (Math/sin (infix Math/PI / 2))))))
 (testing "Symbols"
  (let [x 2, y 3]
   (is (= (* x y) (infix x * y))))))

(deftest bad-syntax
 (testing "Bad syntax"
  (let [valid-infix? #'brave-true-clojure.infix/valid-infix?]
   (is (not (valid-infix? ['+])))
   (is (not (valid-infix? [])))
   (is (not (valid-infix? [[]])))
   (is (not (valid-infix? [[] '+ 1])))
   (is (not (valid-infix? [1 - 2 '+])))
   (is (not (valid-infix? ['+ 1 - 2])))
   (is (not (valid-infix? [1 2])))
   (is (not (valid-infix? [1 [2]])))
   (is (not (valid-infix? [[1] 2]))))))