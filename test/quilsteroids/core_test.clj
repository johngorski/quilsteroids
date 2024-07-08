(ns quilsteroids.core-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.core :refer :all]))

(deftest math
  (testing "norm-squared"
    (is (= 25 (norm-squared [3 4])))
    (is (= 169 (norm-squared [5 12]))))
  (testing "`within` maps value into range from 0 to max"
    (is (= 6. ((within 10) 6)))
    (is (= 6. ((within 10) 16)))
    (is (= 4. ((within 10) -6)))
    (is (= 4. ((within 10) -16))))
  (testing "vector math"
    (testing "subtraction"
      (is (= (v- [0 1] [1 2])
             [-1 -1])))))

(deftest starting-conditions
  (testing "initial state has"
    (let [start (process-events initial-state)]
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
        (is (map? (:ship start)))))))

(def ship
  {:angle 0
   :position [320 240]
   :velocity [0 0]
   :ammo 4})

(def asteroids
  {#uuid "35a4b7c8-1f7d-4c36-9881-98282752bda8"
   {:position [-118.15967179335269 48.308438707101594]
    :velocity [0.8810229126070453 -0.5073853585982386]
    :angle 4.986313423995162
    :angular-velocity -0.14231994510291593
    :mass 3}

   #uuid "1dada50c-6ba8-477b-a774-b387a8d60e99"
   {:position [138.2345410950835 -135.8753662820157]
    :velocity [-0.22163359996566134 -1.3863535825785367]
    :angle 2.715983400080899
    :angular-velocity -0.14459712349828444
    :mass 3}

   #uuid "e01285b9-fb65-402b-acb1-3f24ed24eb95"
   {:position [-225.0313809981529 -71.84416146735212]
    :velocity [1.103630799546257 -0.9923587742431569]
    :angle 2.6475792784539887
    :angular-velocity -0.11394529523479174
    :mass 3}

   #uuid "ca4e9044-8c4b-49e3-9d44-1bd91196d4ef"
   {:position [-193.08068859365804 -101.03367847888627]
    :velocity [0.12455158944010837 -1.990315415163615]
    :angle 0.39488833076814006
    :angular-velocity 0.06691916938941622
    :mass 3}})

(deftest torus-positions
  (testing "for ship"
    (testing "are singular"
      (testing "in the center"
        ;; TODO: torus positions should depend on object protocol.
        ;; Requirements: (position) to get position, (radius) to calculate where to represent.
        ;; Mmmm...doesn't work for lasers. Rethink here.
        (is (= 1 (count (ship-torus-positions {:position (mapv #(* 1/2 %) play-area)})))))
      (testing "just inside center"
        (is (= 1 (count (ship-torus-positions {:position [10 10]}))))))
    (testing "corner ship mirrored everywhere"
      (is (= 4 (count (ship-torus-positions {:position [9 9]}))))
      (is (= 4 (count (ship-torus-positions {:position [1 1]}))))))
  (testing "for laser"
    (testing "tail crosses horizontal boundary"
      (is (= (laser-torus-positions {:position [1 2]
                                     :velocity [5 5]})
             #{[1 2] [641 482]}))))
  (testing "for asteroids"
    (testing "corner"
      (is (= (asteroid-torus-positions {:position [0 0] :mass 3})
             #{[0 0] [640 0] [640 480] [0 480]})))
    (testing "horizontal edge"
      (is (= (asteroid-torus-positions {:position [320 0] :mass 3})
             #{[320 0] [320 480]})))
    (testing "near horizontal edge"
      (is (= (asteroid-torus-positions {:position [320 2] :mass 3})
             #{[320 482] [320 2]})))
    (testing "near left edge"
      (is (= (asteroid-torus-positions {:position [2 240] :mass 3})
             #{[642 240] [2 240]})))
    (testing "near right edge"
      (is (= (asteroid-torus-positions {:position [632 240] :mass 3})
             #{[-8 240] [632 240]})))))

(deftest lasers
  (testing "exhaust"
    (is (= {:lasers {9 :keep}} ((effects [:exhaust-laser 10]) {:lasers {9 :keep 10 :exhaust}})))))

(deftest utils
  (testing "map-values example"
    (is (= (map-values inc {:a 1 :c 3})
           {:a 2, :c 4})))
  (testing "filter-values example"
    (is (= (filter-values odd? {:a 1 :b 2 :c 3})
           {:a 1, :c 3}))))

(deftest smash
  (testing "ship-asteroid"
    (testing "non-collision evaluates"
      (is (empty? (collisions-ship-asteroid ship asteroids)))))
  (testing "asteroid-laser"
    (testing "empty case"
      (is (= (collisions-asteroid-laser {} {})
             {:asteroids #{}, :lasers #{}})))
    (testing "collision"
      (is (= (collisions-asteroid-laser {1 {:mass 3 :position [0 0]}}
                                        {10 {:position [5 5]
                                             :velocity [0 10]}})
             {:asteroids #{1}, :lasers #{10}})))
    (testing "non-collision"
      (is (= (collisions-asteroid-laser {1 {:mass 3 :position [100 0]}}
                                        {10 {:position [5 5]
                                             :velocity [0 10]}})
             {:asteroids #{}, :lasers #{}}))))
  (testing "overall state collision detection"
    (testing "laser hits asteroid"
      (is (= (detect-collisions
              {:asteroids {1 {:mass 3 :position [0 0]}}
               :lasers {10 {:position [5 5]
                            :velocity [0 10]}}
               :ship {:position [200 200]}})
             {:asteroids {1 {:mass 3, :position [0 0]}}
              :lasers {10 {:position [5 5], :velocity [0 10]}}
              :ship {:position [200 200]}
              :events '([:exhaust-laser 10] [:split-asteroid 1])})))))

(deftest events
  (testing "shooting"
    (testing "from initial state"
      (is (= (vals (:lasers (shoot (spawn-ship initial-state))))
             '({:position [330.0 240.0], :velocity [10.0 0.0], :countdown 30})
             )))))

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
    (testing "enact effect of splitting asteroid by id"
      (is (= (->> ((effects [:split-asteroid 3]) {:asteroids {3 {:mass 2 :position [1 1]}}})
                  :asteroids
                  vals
                  (map #(select-keys % [:position :mass])))
             '({:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}
               {:position [1 1], :mass 1.0} {:position [1 1], :mass 1.0}))))))
