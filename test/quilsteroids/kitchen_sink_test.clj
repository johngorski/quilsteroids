(ns quilsteroids.kitchen-sink-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.kitchen-sink :refer :all]
   ))

(deftest utils
  (testing "map-values example"
    (is (= (map-values inc {:a 1 :c 3})
           {:a 2, :c 4})))
  (testing "filter-values example"
    (is (= (filter-values odd? {:a 1 :b 2 :c 3})
           {:a 1, :c 3}))))

