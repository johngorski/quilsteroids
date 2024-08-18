(ns quilsteroids.ship-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.game :as game]
   [quilsteroids.object :as object]
   [quilsteroids.ship :refer :all]
   ))

(defn game-positions [o]
  (object/visible-positions o {:geo game/game-torus :viewport [[0 640] [0 480]]}))

(deftest torus-positions
  (testing "for ship"
    (testing "are singular"
      (testing "in the center"
        ;; TODO: torus positions should depend on object protocol.
        ;; Requirements: (position) to get position, (radius) to calculate where to represent.
        ;; Mmmm...doesn't work for lasers. Rethink here.
        (is (= 1 (count (game-positions (ship-at (mapv #(* 1/2 %) game/play-area)))))))
      (testing "just inside center"
        (is (= 1 (count (game-positions (ship-at [10 10])))))))
    (testing "corner ship mirrored everywhere"
      (is (= 4 (count (game-positions (ship-at [9 9])))))
      (is (= 4 (count (game-positions (ship-at [1 1]))))))))
