(ns quilsteroids.game-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.asteroid :as asteroid]
   [quilsteroids.game :refer :all]
   [quilsteroids.laser :as laser]
   [quilsteroids.ship :as ship]
   ))

(def start (process-events initial-state))

(deftest starting-conditions
  (testing "initial state has"
    (testing "empty"
      (testing "controls"
        (let [ctrl (:controls start)]
          (is (set? ctrl))
          (is (empty? ctrl))))
      (testing "lasers"
        (let [lasers (:lasers start)]
          (is (map? lasers))
          (is (empty? lasers))))
      (testing "event queue"
        (is (empty? (:events start)))))
    (testing "asteroids"
      (let [asteroids (:asteroids start)]
        (is (= 4 (count asteroids)))
        (is (map? asteroids))))
    (testing "ship"
      (is (map? (:ship start))))))

(def ship
  (ship/map->Ship {:angle 0
                   :position [320 240]
                   :velocity [0 0]
                   :ammo 4}))

(def asteroids
  {#uuid "35a4b7c8-1f7d-4c36-9881-98282752bda8"
   (asteroid/map->Asteroid
    {:position [-118.15967179335269 48.308438707101594]
     :velocity [0.8810229126070453 -0.5073853585982386]
     :angle 4.986313423995162
     :angular-velocity -0.14231994510291593
     :mass 3})

   #uuid "1dada50c-6ba8-477b-a774-b387a8d60e99"
   (asteroid/map->Asteroid
    {:position [138.2345410950835 -135.8753662820157]
     :velocity [-0.22163359996566134 -1.3863535825785367]
     :angle 2.715983400080899
     :angular-velocity -0.14459712349828444
     :mass 3})

   #uuid "e01285b9-fb65-402b-acb1-3f24ed24eb95"
   (asteroid/map->Asteroid
    {:position [-225.0313809981529 -71.84416146735212]
     :velocity [1.103630799546257 -0.9923587742431569]
     :angle 2.6475792784539887
     :angular-velocity -0.11394529523479174
     :mass 3})

   #uuid "ca4e9044-8c4b-49e3-9d44-1bd91196d4ef"
   (asteroid/map->Asteroid
    {:position [-193.08068859365804 -101.03367847888627]
     :velocity [0.12455158944010837 -1.990315415163615]
     :angle 0.39488833076814006
     :angular-velocity 0.06691916938941622
     :mass 3})})

(deftest lasers
  (testing "exhaust"
    (is (= {:lasers {9 :keep}}
           ((effects [:exhaust-laser 10]) {:lasers {9 :keep 10 :exhaust}})))))

(deftest smash
  (testing "ship-asteroid"
    (testing "non-collision evaluates"
      (is (empty? (collisions-ship-asteroid ship asteroids)))))
  (testing "asteroid-laser"
    (testing "empty case"
      (is (= (collisions-asteroid-laser {} {})
             {:asteroids #{}, :lasers #{}})))
    (testing "collision"
      (is (= (collisions-asteroid-laser {1 #quilsteroids.asteroid.Asteroid{:mass 3 :position [0 0]}}
                                        {10 #quilsteroids.laser.Laser{:position [5 5]
                                             :velocity [0 10]}})
             {:asteroids #{1}, :lasers #{10}})))
    (testing "non-collision"
      (is (= (collisions-asteroid-laser {1 (asteroid/map->Asteroid {:mass 3 :position [100 0]})}
                                        {10 #quilsteroids.laser.Laser{:position [5 5]
                                             :velocity [0 10]}})
             {:asteroids #{}, :lasers #{}}))))
  (testing "overall state collision detection"
    (testing "laser hits asteroid"
      (is (= (detect-collisions
              {:asteroids {1 (asteroid/map->Asteroid {:mass 3 :position [0 0]})}
               :lasers {10 #quilsteroids.laser.Laser{:position [5 5]
                            :velocity [0 10]}}
               :ship #quilsteroids.ship.Ship{:position [200 200]}})
             {:asteroids {1 (asteroid/map->Asteroid {:mass 3, :position [0 0]})}
              :lasers {10 #quilsteroids.laser.Laser{:position [5 5], :velocity [0 10]}}
              :ship #quilsteroids.ship.Ship{:position [200 200]}
              :events '([:exhaust-laser 10] [:split-asteroid 1])})))))

(deftest events
  (testing "shooting"
    (testing "from initial state"
      (is (= (vals (:lasers (shoot ((ship/spawn-ship-at [320 240]) initial-state))))
             [(laser/map->Laser {:position [330.0 240.0], :velocity [10.0 0.0], :countdown 30})]
             )))))

(deftest asteroid-behavior
  (testing "examples"
    (testing "enact effect of splitting asteroid by id"
      (is (= (->> ((effects [:split-asteroid 3]) {:asteroids {3 {:mass 2 :position [1 1]}}})
                  :asteroids
                  vals
                  (map #(select-keys % [:position :mass])))
             '({:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}
               {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}))))))

(deftest game-launch
  (testing "initial state"
    (testing "first update succeeds"
      (is (map? (update-state initial-state))))))
