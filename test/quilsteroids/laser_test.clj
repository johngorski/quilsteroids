(ns quilsteroids.laser-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.game :as game]
   [quilsteroids.geometry :as geometry]
   [quilsteroids.laser :refer :all]
   [quilsteroids.object :as object]
   ))

(def test-torus-width 200)
(def test-torus-height 100)
(def test-torus-middle (map #(* 1/2 %) [test-torus-width test-torus-height]))
(def test-field (geometry/torus test-torus-width test-torus-height))

(deftest visibility
  (testing "visible laser positions"
    (testing "one, right in the middle"
      (is (= #{test-torus-middle}
             (object/visible-positions
              (map->Laser {:position test-torus-middle,
                           :velocity [0 10],
                           :countdown nil})
              {:geo test-field
               :viewport [[0 test-torus-width] [0 test-torus-height]]}
              ))))))

(defn game-positions [o]
  (object/visible-positions o {:geo game/game-torus :viewport [[0 640] [0 480]]}))

(deftest torus-positions
  (testing "for laser"
    (testing "tail crosses horizontal boundary"
      (is (= (game-positions (map->Laser {:position [1 2]
                                          :velocity [5 5]}))
             #{[1 2] [641 482]})))))
