(ns quilsteroids.asteroid-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.asteroid :refer :all]
   [quilsteroids.game :as game]
   [quilsteroids.geometry :as geometry]
   [quilsteroids.object :as object]
   ))

(deftest asteroid-behavior
  (testing "examples"
    (testing "smaller asteroids"
      (is (= (map #(select-keys % [:position :mass]) (smaller-asteroids {:mass 2 :position [1 1]}))
             '({:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}
               {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}
               {:position [1 1], :mass 1.0}))))
    (testing "smaller than smallest"
      (is (empty? (smaller-asteroids {:mass 1 :position [1 1]}))))
    (testing "split asteroid by id"
      (is (= (map #(select-keys % [:position :mass])
                  (vals (:asteroids (split-asteroid {:asteroids {3 {:mass 2 :position [1 1]}}} 3))))
             '({:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}
               {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}))))
    ))

(deftest asteroid-visibility
  (testing "correct visibility checking"
    (is (visible?
         (map->Asteroid {:position [-118.15967179335269 48.308438707101594],
                         :velocity [0.8810229126070453 -0.5073853585982386],
                         :angle 4.986313423995162,
                         :angular-velocity -0.14231994510291593,
                         :mass 3})
         {:when-at [521.8403282066473 431.6915612928984],
          :within '([0 640] [0 480])}))))

(defn game-positions [o]
  (object/visible-positions o {:geo game/game-torus :viewport [[0 640] [0 480]]}))

(deftest torus-positions
  (testing "for asteroids"
    (testing "corner"
      (is (= (game-positions (map->Asteroid {:position [0 0] :mass 3}))
             #{[0 0] [640 0] [640 480] [0 480]})))
    (testing "horizontal edge"
      (is (= (game-positions (map->Asteroid {:position [320 0] :mass 3}))
             #{[320 0] [320 480]})))
    (testing "near horizontal edge"
      (is (= (game-positions (map->Asteroid {:position [320 2] :mass 3}))
             #{[320 482] [320 2]})))
    (testing "near left edge"
      (is (= (game-positions (map->Asteroid {:position [2 240] :mass 3}))
             #{[642 240] [2 240]})))
    (testing "near right edge"
      (is (= (game-positions (map->Asteroid {:position [632 240] :mass 3}))
             #{[-8 240] [632 240]})))))

