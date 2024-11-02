(ns quilsteroids.geometry-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.geometry :refer :all]))

(deftest bounds
  (testing "bounds"
    (testing "examples"
      (is (within? [[0 640] [0 480]] [320 240]))
      (is (not (within? [[0 640] [0 480]] [820 240]))))))

;; TODO: Proper spec'ing and generative testing
(deftest generative
  (testing "randomly sampled"
    (testing "angle"
      (is (<= 0 (rand-angle) (* 2 Math/PI))))
    (testing "boundary"
      (is (<= 1/2 (rand-bounded 1/2 2) 2)))))


