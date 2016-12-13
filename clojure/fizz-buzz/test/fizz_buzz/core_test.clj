(ns fizz-buzz.core-test
  (:require [clojure.test :refer :all]
            [fizz-buzz.core :refer :all]))

(deftest normal-numbers
  (testing "Normal numbers should be returned as is."
    (is (= "1" (generate 1)))
    (is (= "2" (generate 2)))
    ))

(deftest fizz-numbers
  (testing "Numbers divisible by 3 should return Fizz."
    (is (= "Fizz" (generate 3)))
    (is (= "Fizz" (generate 6)))
    ))

(deftest buzz-numbers
  (testing "Numbers divisible by 5 should return Buzz."
    (is (= "Buzz" (generate 5)))
    (is (= "Buzz" (generate 10)))
    ))

(deftest fizzbuzz-numbers
  (testing "Numbers divisible by both 3 and 5 should return Fizz-Buzz."
    (is (= "Fizz-Buzz" (generate 15)))
    (is (= "Fizz-Buzz" (generate 30)))
    ))

