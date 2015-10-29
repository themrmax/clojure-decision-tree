(ns decisiontree.core-test
  (:require [clojure.test :refer :all]
            [decisiontree.core :refer :all]))

(def data-path "data/modelling_data.csv")
(def data (load-csv data-path))


(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest load-csv-notnil
  (testing "testing that the csv loads not null"
    (is (not= (load-csv "data/modelling_data.csv") nil))))

(deftest response-rate-test
  (testing "testing response rate on a basic example"
    (is (= (response-rate '([1 1] [0 0] [0 1] [0 1])) 0.25))))

(deftest counts-and-response-rates-test
  (testing "testing response-rates-and-counts"
    (is (= (counts-and-response-rates '([1 "a"] [0 "a"] [1 "b"]) 1) '(["a" 2 0.5] ["b" 1 1.0])))))

(deftest cast-first-as-int-basic-test
  (testing "testing that a basic example")
    (is (= (cast-first-as-int ["1" "a" "b"]) [1 "a" "b"] )))

(deftest get-best-split-of-variable-test
  (testing "testing splitting on a variable")
  (is (= (get-best-split-of-variable [[1 1] [1 1] [1 1] [1 1] [1 2] [0 2] [0 2] [0 3] [0 3] [0 3]] 1) [2 0.13888889220025802])))

(deftest split-returns-two-lists
  (testing "testing that split returns two list"
    (is (= (count (split (load-csv "data/modelling_data.csv"))) 2))))

(deftest split-result-total-length-equals-length-of-input
  (testing "testing that split returns add up to total length of input"
    (is (= (reduce + (map count (split data) ))
             (count data)))))
