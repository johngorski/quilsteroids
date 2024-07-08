(ns quilsteroids.core
  (:require
   [clojure.set :as sets]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import (clojure.lang PersistentQueue)))

;; TODOs (refactoring throughout, tests overdue)
;; - CLEARLY CLEARLY CLEARLY where some refactoring is mandatory. Protocols/multimethods.
;; - cap ship speed
;; - cracks in asteroids--taking forever, probably not worth doing ever, maybe.

(def play-area
  [640 480])

(defn in-play-area? [[x y]]
  (let [[play-width play-height] play-area]
    (and
     (<= 0 x play-width)
     (<= 0 y play-height))))

(defn within
  "Function which clips the value passed to at least zero and at most the given width"
  [width]
  (fn [x]
    (let [whole-widths (Math/floor (/ x width))]
      (- x (* width whole-widths)))
    ))

(defn on-torus
  "Function which clips the point passed to its argument to the given width and height"
  [[width height]]
  (let [clip-width (within width)
        clip-height (within height)]
    (fn [[x y]]
      [(clip-width x) (clip-height y)])))

(def on-game-torus (on-torus play-area))

(defn rectangular [r theta]
  [(* r (Math/cos theta)) (* r (Math/sin theta))])

(def v+ (partial mapv +))
(def v- (partial mapv -))

(def laser-speed 10)
(def laser-length laser-speed)

(def ship-radius 10)

(def queue PersistentQueue/EMPTY)

(comment
  ;; "crack" asteroids later. Don't worry about it.

  (defn random-cracks
    "Random \"cracks\" for asteroid. Parallel sequences of arcs and heights.
  Dip at most one level down at most once per sequences."
    []
    (repeatedly 5 #(rand-nth [false true])))

  (random-cracks)

  (defn smooth
    "Smooth cracks by making sure you don't have two cracks in a row (wrapping around). Add non-cracks to smooth out."
    [cracks]))

(def initial-ship
  {:angle 0
   :position (mapv #(/ % 2) play-area)
   :velocity [0 0]
   :ammo 4})

(defn spawn-ship [state]
  (assoc state :ship initial-ship))

(defn rand-angle []
  (* (rand) 2 Math/PI))

(comment
  (rand-angle))

(defn rand-bounded [least most]
  (+ least (* (rand) (- most least))))

(comment
  (rand-bounded 1/2 2))

;; TODO: Asteroid functions will likely be much more comfortable in their own namespace.

(def min-asteroid-speed 1/2)
(def max-asteroid-speed 2)

(defn rand-asteroid-velocity
  ""
  []
  (rectangular (rand-bounded min-asteroid-speed max-asteroid-speed) (rand-angle)))

(defn random-asteroid
  "Random asteroid, optionally with presets."
  ([presets] (merge (random-asteroid) presets))
  ([]
   (let [height (second play-area)
         min-speed 1/2
         max-speed 2
         full-rotation (* 2 Math/PI)
         max-omega (/ full-rotation 40)
         min-start (/ height 4)
         max-start (/ height 2)]
     {:position (rectangular (rand-bounded min-start max-start) (rand-angle))
      :velocity (rand-asteroid-velocity)
      :angle (rand-angle)
      :angular-velocity (rand-bounded (- max-omega) max-omega)
      :mass 3})))

(comment
  (random-asteroid)
  (random-asteroid {:position [13 37] :mass 1}))

(def object-type-keys {:asteroid :asteroids, :laser :lasers})

(defn add-object
  "Add an obj of type type (:laser / :asteroid) to the game state."
  [state type obj]
  (update state (object-type-keys type) assoc (random-uuid) obj))

(defn remove-object
  "Removes the object with the given id of type type from the game state."
  [state type id]
  (update state (object-type-keys type) dissoc id))

(defn spawn-asteroid
  ([state] (spawn-asteroid state {}))
  ([state asteroid]
   (add-object state :asteroid (merge (random-asteroid) asteroid))))

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

(defn shoot [{:keys [ship] :as state}]
  (if (< (count (:lasers state)) 4)
    (let [fresh-laser {:position (v+ (:position ship)
                                     (rectangular ship-radius (:angle ship)))
                       :velocity (rectangular laser-speed (:angle ship))
                       :countdown 30}]
      (add-object state :laser fresh-laser))
    state))

(defn setup []
  (q/frame-rate 30)
  initial-state)

(defn norm-squared [v]
  (reduce + (map * v v)))

(defn distance-squared
  "distance squared between p1 and p2"
  [p1 p2]
  (norm-squared (map - p1 p2)))

(defn smaller-asteroids
  "Smaller chunks of asteroid"
  [{:keys [position mass] :as asteroid}]
  (if (<= mass 1)
    []
    (let [num-chunks 5
          chunk-mass (dec (Math/floor mass))]
      (repeatedly num-chunks #(random-asteroid {:position position :mass chunk-mass})))))

(defn split-asteroid
  "Replace the given asteroid with new asteroids of smaller mass."
  [state asteroid-id]
  (if-let [asteroid (get-in state [:asteroids asteroid-id])]
    (let [without-asteroid (remove-object state :asteroid asteroid-id)]
      (reduce (fn [state chunk] (add-object state :asteroid chunk))
              without-asteroid
              (smaller-asteroids asteroid)))
    state))

(defn exhaust-laser
  ""
  [state laser-id]
  (remove-object state :laser laser-id))

(defn effects
  "functions from state to state"
  [game-event]
  (cond
    (keyword? game-event)
    (get
     {:shoot shoot
      :respawn spawn-ship
      :spawn-asteroid spawn-asteroid}
     game-event
     identity)

    (seqable? game-event)
    (let [[event-type & args] game-event]
      (case event-type
        :split-asteroid
        (let [[asteroid-id] args]
          (fn [state]
            (split-asteroid state asteroid-id)))
        :exhaust-laser
        (let [[laser-id] args]
          (fn [state]
            (exhaust-laser state laser-id)))))

    :default
    identity))

(defn process-events
  "Process event queue to update the state"
  [{:keys [events] :as state}]
  (assoc
   (reduce #(%2 %1) state (map effects events))
   :events queue)
  )

(def turn-rate (/ Math/PI 20))
(def ship-acceleration 1/4)

(defn actuate-controls [{:keys [controls ship] :as state}]
  (-> state
      (update-in [:ship :angle]
                 #(+ % (* turn-rate
                          (+ (if (:left-turn controls) -1 0)
                             (if (:right-turn controls) 1 0)))))
      (update-in [:ship :velocity]
                 #(v+ % (rectangular
                         (* ship-acceleration (if (:thrusters controls) 1 0))
                         (:angle ship))))))

(defn move-ship [ship]
  (update ship :position #(on-game-torus (v+ % (:velocity ship)))))

(defn move-laser [laser]
  (-> laser
      (update :position #(on-game-torus (v+ % (:velocity laser))))
      (update :countdown dec)))

(defn map-values
  "Applies f to every value in m, leaving keys unchanged. Returns a transducer when m is not provided."
  ([f]
   (map (fn [[k v]] [k (f v)])))
  ([f m]
   (into {} (map-values f) m)))

(defn filter-values
  "Filters m according to the trutiness of (f v) for the value v of each entry in m.
  Returns a transducer when m is not provided."
  ([f]
   (filter (fn [[k v]] (f v))))
  ([f m]
   (into {} (filter-values f) m)))

(defn move-lasers [state]
  (update state :lasers #(into {}
                               (comp
                                (map-values move-laser)
                                (filter-values (fn [laser] (< 0 (:countdown laser)))))
                               %)))

(defn move-asteroid [{:keys [velocity angular-velocity] :as asteroid}]
  (-> asteroid
      (update :position #(on-game-torus (v+ % velocity)))
      (update :angle #(+ % angular-velocity))))

(defn move-asteroids [state]
  (update state :asteroids #(map-values move-asteroid %)))

(defn move-objects [state]
  (-> state
      move-asteroids
      move-lasers
      (update :ship move-ship)
      ))

(def torus-translations
  (let [[width height] play-area]
    [[(- width) (- height)] [0 (- height)] [width (- height)]
     [(- width)    0      ] [0    0      ] [width    0      ]
     [(- width)    height ] [0    height ] [width    height ]]))

(defn torus-points [p]
  (map #(v+ p %) torus-translations))

(defn ship-torus-positions
  "Cycle supply vastly outstrips cycle demand. Let's see how this goes if we render the full
  cross of everything.

  But...naive collision detection is O(n^2), so it's worth keeping n low (i.e. not 9x) since a
  9x change in position/update/drawing becomes 81x in collision detection.

  Torus positions are important here so that we can track one position for moving the ship around,
  but then we can check all *torus* positions for collisions and draw at all torus positions as well.

  Some form of this will likely get generalized to all objects on torus geometry."
  [ship]
  (let [[width height] play-area]
    (into #{}
          (comp
           (map #(v+ (:position ship) %))
           (filter (fn [[x y]]
                     (and
                      (<= (- ship-radius) x) (< x (+ width ship-radius))
                      (<= (- ship-radius) y) (< y (+ height ship-radius))))))
          torus-translations
          )))

(defn draw-ship [{:keys [angle] :as ship}]
  (q/stroke 200)
  (doseq [p (ship-torus-positions ship)]
    (q/with-translation p
      (let [R ship-radius
            r (* 7/10 R)
            delta (* 5/6 Math/PI)
            left-angle (+ angle delta)
            right-angle (- angle delta)
            nose (rectangular R angle)
            left-tip (rectangular R left-angle)
            left-base (rectangular r left-angle)
            right-tip (rectangular R right-angle)
            right-base (rectangular r right-angle)
            tail (rectangular R (+ angle Math/PI))]
        (q/line nose left-tip)
        (q/line nose right-tip)
        (q/line left-base right-base)
        (when (:thrusting? ship)
          (q/line left-base tail)
          (q/line right-base tail))))))

(defn laser-ends [laser]
  (let [{:keys [position velocity]} laser]
    {:tip position
     :tail (v- position velocity)}))

(defn laser-torus-positions
  "Positions at which to draw lasers and detect collisions"
  [{:keys [position velocity] :as laser}]
  (into #{}
        (filter (fn [p]
                  (or (in-play-area? p)
                      (in-play-area? (v- p velocity)))))
        (torus-points position)))

(defn draw-laser [{:keys [position velocity] :as laser}]
  (q/stroke 255)
  (doseq [p (laser-torus-positions laser)]
    (q/with-translation p
      (q/line [0 0] (mapv - velocity)))))

(defn asteroid-radius [asteroid]
  (* 10 (:mass asteroid)))

(defn asteroid-torus-positions [asteroid]
  (let [[width height] play-area
        major-radius (asteroid-radius asteroid)]
    (into #{}
          (filter (fn [[x y]]
                    (and
                     (<= (- major-radius) x) (< x (+ width major-radius))
                     (<= (- major-radius) y) (< y (+ height major-radius)))))
          (torus-points (:position asteroid)))))

(defn asteroid-laser-collided? [asteroid laser]
  (let [r (asteroid-radius asteroid)
        boundary-squared (* r r)]
    (first
     (for [a-p (asteroid-torus-positions asteroid)
           l-p (laser-torus-positions laser)
           :when (< (distance-squared a-p l-p) boundary-squared)]
       [a-p l-p]))))

(defn collisions-asteroid-laser [asteroids lasers]
  (reduce #(merge-with conj %1 %2)
          {:asteroids #{}
           :lasers #{}}
          (for [[asteroid-id asteroid] asteroids
                [laser-id laser] lasers
                :when (asteroid-laser-collided? asteroid laser)]
            {:asteroids asteroid-id
             :lasers laser-id}
            )))

(defn laser-asteroid-collision-events [collided-asteroids-and-lasers]
  (concat
   (map (fn [asteroid-id]
          [:split-asteroid asteroid-id])
        (:asteroids collided-asteroids-and-lasers))
   (map (fn [laser-id]
          [:exhaust-laser laser-id])
        (:lasers collided-asteroids-and-lasers))))

(defn ship-asteroid-collided? [ship asteroid]
  (let [r (asteroid-radius asteroid)
        boundary (+ r ship-radius)
        boundary-squared (* boundary boundary)]
    (first
     (for [a-p (asteroid-torus-positions asteroid)
           s-p (ship-torus-positions ship)
           :when (< (distance-squared a-p s-p) boundary-squared)]
       [s-p a-p]))))

(defn collisions-ship-asteroid
  "Set of asteroid IDs into which the ship has collided"
  [ship asteroids]
  (into #{}
        (for [[asteroid-id asteroid] asteroids
              :when (ship-asteroid-collided? ship asteroid)]
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

(defn draw-asteroid [{:keys [position angle mass] :as asteroid}]
  (q/stroke 180)
  (let [segments 9
        major-radius (* 10 mass)
        minor-radius (* 10 (dec mass))
        arc-angle (/ (* 2 Math/PI) segments)
        angles (range 0 (* 2 Math/PI) arc-angle)
        [inner & outers] angles]
    (doseq [p (asteroid-torus-positions asteroid)]
      (q/with-translation p
        (q/with-rotation [angle]
          (doseq [[t1 t2] (partition 2 1 outers)]
            (q/line (rectangular major-radius t1) (rectangular major-radius t2)))
          (let [inner-p (rectangular minor-radius inner)]
            (q/line inner-p (rectangular major-radius (first outers)))
            (q/line inner-p (rectangular major-radius (last outers)))))))))

(defn draw-state [{:keys [asteroids controls lasers ship] :as state}]
  (q/background 0)
  (draw-ship (assoc ship :thrusting? (:thrusters controls)))
  (doseq [laser (vals lasers)] (draw-laser laser))
  (doseq [asteroid (vals asteroids)] (draw-asteroid asteroid)))

(def held-controls
  {:up :thrusters
   :left :left-turn
   :right :right-turn})

(def triggers
  {:shift :shoot
   :down :warp})

(defn control-held [old-state event]
  (if-let [held-control (held-controls (:key event))]
    (update old-state :controls conj held-control)
    old-state))

(defn trigger-squeezed [old-state event]
  (if-let [squeezed-trigger (triggers (:key event))]
    (update old-state :events conj squeezed-trigger)
    old-state))

(defn key-pressed [old-state event]
  (-> old-state
      (control-held event)
      (trigger-squeezed event)))

(defn key-released [old-state event]
  (if-let [released-control (held-controls (:key event))]
    (update old-state :controls disj released-control)
    old-state))

(defn update-state [state]
  (-> state
      detect-collisions
      process-events
      actuate-controls
      move-objects
      ))

(comment
  (q/defsketch quilsteroids
    ;; This sketch uses functional-mode middleware.
    ;; Check quil wiki for more info about middlewares and particularly
    ;; fun-mode.
    :middleware [m/fun-mode]
    :features [:keep-on-top]
    :title "Quilsteroids!"
    :size play-area
    ;; setup function called only once, during sketch initialization.
    :setup setup
    ;; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :key-pressed key-pressed
    :key-released key-released
    ))
