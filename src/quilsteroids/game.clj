(ns quilsteroids.game
  (:gen-class)
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   ;; TODO: Remove unused namespace references after accomplishing intended inversions
   [quilsteroids.asteroid :as asteroid]
   [quilsteroids.laser :as laser]
   [quilsteroids.ship :as ship]
   ;; end TODO
   [quilsteroids.controls :as controls]
   [quilsteroids.geometry :as geometry :refer [v+]]
   [quilsteroids.kitchen-sink :refer [filter-values map-values queue]]
   [quilsteroids.object :as object]
   ))

(def play-area
  [640 480])

(def game-torus (apply geometry/torus play-area))

(defn in-play-area? [point]
  (let [[play-width play-height] play-area]
    (geometry/within? [[0 play-width]
                       [0 play-height]]
                      point)))

(def initial-state
  {:controls #{}
   :events (conj queue
                 :respawn
                 :spawn-asteroid
                 :spawn-asteroid
                 :spawn-asteroid
                 :spawn-asteroid)

   :lasers {}

   :asteroids {}
   })

;; TODO: Move to ship namespace
(defn shoot [{:keys [ship] :as state}]
  (if (< (count (:lasers state)) 4)
    (object/add-object state :laser (ship/fresh-laser ship))
    state))

(defn effects
  "An effect is a function from state to state. game-events may yield several effects.
  TODO: Model all effects this way. Eliminate ersatz functions which take a state and something else.
  TODO: Replace with multimethod."
  [game-event]
  (cond
    (keyword? game-event)
    (get
     {:shoot shoot
      :respawn (let [center (mapv #(/ % 2) play-area)]
                 (ship/spawn-ship-at center))
      :spawn-asteroid (let [minor-axis (second play-area)
                            min-dist (* 1/4 minor-axis)
                            max-dist (* 3/4 minor-axis)
                            ]
                        (asteroid/spawn-asteroid-at (geometry/rectangular
                                                     (geometry/rand-bounded min-dist max-dist)
                                                     (geometry/rand-angle)
                                                     )))}
     game-event
     identity)

    (seqable? game-event)
    (let [[event-type & args] game-event]
      (case event-type
        :split-asteroid
        (let [[asteroid-id] args]
          (fn [state]
            (asteroid/split-asteroid state asteroid-id)))
        :exhaust-laser
        (let [[laser-id] args]
          (fn [state]
            (laser/exhaust-laser state laser-id)))))

    :default
    identity))

(defn process-events
  "Process event queue to update the state"
  [{:keys [events] :as state}]
  (assoc
   (reduce #(%2 %1) state (map effects events))
   :events queue))

(defn move-objects [state]
  (-> state
      (update :asteroids (fn [asteroids]
                           (map-values (fn [asteroid]
                                         (object/move asteroid game-torus)) asteroids)))
      (update :lasers #(into {}
                             (comp
                              (map-values (fn [laser]
                                            (object/move laser game-torus)))
                              (filter-values (fn [laser]
                                               (< 0 (:countdown laser)))))
                             %))
      (update :ship #(object/move % game-torus))
      ))

(def play-viewport
  (map (fn [end]
         [0 end])
       play-area))

(defn visible-positions [o]
  (object/visible-positions o
                            {:geo game-torus
                             :viewport play-viewport}))

(defn by-class [this that]
  {(class this) this
   (class that) that})

#_(by-class (laser/map->Laser {}) (asteroid/map->Asteroid {}))

(defmulti collided? (fn [this that]
                      #{(class this) (class that)}))

(defn asteroid-laser-collided? [asteroid laser]
  (let [r (asteroid/asteroid-radius asteroid)
        boundary-squared (* r r)
        viewport-bounds play-viewport]
    (first
     (for [a-p (visible-positions asteroid)
           l-p (visible-positions laser)
           :when (< (geometry/distance-squared a-p l-p) boundary-squared)]
       [a-p l-p]))))

(defmethod collided? #{quilsteroids.asteroid.Asteroid
                       quilsteroids.laser.Laser}
  [this that]
  (let [[asteroid laser]
        (map (by-class this that)
             [quilsteroids.asteroid.Asteroid
              quilsteroids.laser.Laser])]
    (asteroid-laser-collided? asteroid laser)))

(defn collisions-asteroid-laser [asteroids lasers]
  (reduce #(merge-with conj %1 %2)
          {:asteroids #{}
           :lasers #{}}
          (for [[asteroid-id asteroid] asteroids
                [laser-id laser] lasers
                :when (collided? asteroid laser)]
            {:asteroids asteroid-id
             :lasers laser-id})))

(defn laser-asteroid-collision-events [collided-asteroids-and-lasers]
  (concat
   (map (fn [asteroid-id]
          [:split-asteroid asteroid-id])
        (:asteroids collided-asteroids-and-lasers))
   (map (fn [laser-id]
          [:exhaust-laser laser-id])
        (:lasers collided-asteroids-and-lasers))))

(defn ship-asteroid-collided? [ship asteroid]
  (let [r (asteroid/asteroid-radius asteroid)
        boundary (+ r ship/ship-radius)
        boundary-squared (* boundary boundary)]
    (first
     (for [a-p (visible-positions asteroid)
           s-p (visible-positions ship)
           :when (< (geometry/distance-squared a-p s-p) boundary-squared)]
       [s-p a-p]))))

(defmethod collided? #{quilsteroids.asteroid.Asteroid
                       quilsteroids.ship.Ship}
  [this that]
  (let [[ship asteroid]
        (map (by-class this that)
             [quilsteroids.ship.Ship
              quilsteroids.asteroid.Asteroid])]
    (ship-asteroid-collided? ship asteroid)))

(defn collisions-ship-asteroid
  "Set of asteroid IDs into which the ship has collided"
  [ship asteroids]
  (into #{}
        (for [[asteroid-id asteroid] asteroids
              :when (collided? ship asteroid)]
          asteroid-id)))

(defn ship-asteroid-collision-events [collided-asteroids]
  (if (empty? collided-asteroids)
    []
    (concat [:respawn]
            (map (fn [asteroid-id] [:split-asteroid asteroid-id]) collided-asteroids))))

(defn detect-collisions
  "Enqueue events/transform state based on collisions in current state"
  [state]
  (let [asteroids (:asteroids state)
        laser-asteroid-collisions (collisions-asteroid-laser asteroids (:lasers state))
        ship-asteroid-collisions (collisions-ship-asteroid (:ship state) asteroids)
        ]
    (update state :events #(reduce conj % (concat
                                           (laser-asteroid-collision-events laser-asteroid-collisions)
                                           (ship-asteroid-collision-events ship-asteroid-collisions))))))

(defn draw-object [o]
  (doseq [p (object/visible-positions o {:geo game-torus
                                         :viewport play-viewport})]
    (object/draw o p)))

(defn draw-state [{:keys [asteroids controls lasers ship] :as state}]
  (q/background 0)
  (draw-object (assoc ship :thrusting? (:thrusters controls)))
  (doseq [laser (vals lasers)] (draw-object laser))
  (doseq [asteroid (vals asteroids)] (draw-object asteroid)))

(defn actuate-controls [{:keys [controls ship] :as state}]
  (-> state
      (update-in [:ship :angle]
                 #(+ % (* ship/turn-rate
                          (+ (if (:left-turn controls) -1 0)
                             (if (:right-turn controls) 1 0)))))
      (update-in [:ship :velocity]
                 #(v+ % (geometry/rectangular
                         (* ship/ship-acceleration (if (:thrusters controls) 1 0))
                         (:angle ship))))))

(defn update-state [state]
  (-> state
      detect-collisions
      process-events
      actuate-controls
      move-objects
      ))

(defn setup []
  (q/frame-rate 30)
  initial-state)

(defn -main [& args]
  (q/defsketch quilsteroids
    ;; This sketch uses functional-mode middleware.
    ;; Check quil wiki for more info about middlewares and particularly
    ;; fun-mode.
    :middleware [m/fun-mode]
    :features [:keep-on-top]
    :title "Quilsteroids! - game namespace"
    :size play-area
    ;; setup function called only once, during sketch initialization.
    :setup setup
    ;; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :key-pressed controls/key-pressed
    :key-released controls/key-released
    ))
