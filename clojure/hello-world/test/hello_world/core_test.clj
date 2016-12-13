(ns hello-world.core-test
  (:require [clojure.test :refer :all]
            [hello-world.core :refer :all]))

(deftest a-test
  (testing "I succeed."
    (is (= true true))))

(deftest a-second-test
  (testing "I fail."
    (is (= true false))))
