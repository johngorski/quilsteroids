(ns quilsteroids.core-test
  (:require
   [clojure.test :refer :all]
   [quilsteroids.core :refer :all]))

(comment
  (process-events initial-state)
  {:controls #{}
   ;; :events #object[clojure.lang.PersistentQueue 0x1e122a42 "clojure.lang.PersistentQueue@1"]
   :lasers {}
   :asteroids {#uuid "35a4b7c8-1f7d-4c36-9881-98282752bda8" {:position [-118.15967179335269 48.308438707101594]
                                                             :velocity [0.8810229126070453 -0.5073853585982386]
                                                             :angle 4.986313423995162
                                                             :angular-velocity -0.14231994510291593
                                                             :mass 3}
               #uuid "1dada50c-6ba8-477b-a774-b387a8d60e99" {:position [138.2345410950835 -135.8753662820157]
                                                             :velocity [-0.22163359996566134 -1.3863535825785367]
                                                             :angle 2.715983400080899
                                                             :angular-velocity -0.14459712349828444
                                                             :mass 3}
               #uuid "e01285b9-fb65-402b-acb1-3f24ed24eb95" {:position [-225.0313809981529 -71.84416146735212]
                                                             :velocity [1.103630799546257 -0.9923587742431569]
                                                             :angle 2.6475792784539887
                                                             :angular-velocity -0.11394529523479174
                                                             :mass 3}
               #uuid "ca4e9044-8c4b-49e3-9d44-1bd91196d4ef" {:position [-193.08068859365804 -101.03367847888627]
                                                             :velocity [0.12455158944010837 -1.990315415163615]
                                                             :angle 0.39488833076814006
                                                             :angular-velocity 0.06691916938941622
                                                             :mass 3}}
   :ship {:angle 0
          :position [320 240]
          :velocity [0 0]
          :ammo 4}}
  ())

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
      (is (= 4 (count (ship-torus-positions {:position [1 1]})))))))

(deftest smash-ship-asteroid
  (testing "non-collision evaluates"
    (is (empty? (collisions-ship-asteroid ship asteroids)))))

