(ns quilsteroids.core
  (:require
   [clojure.set :as sets]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import (clojure.lang PersistentQueue)))

;; TODOs (refactoring throughout, tests overdue)
;; - collisions
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

;; TODO: into unit tests
(comment
  ((within 10) 6)
  ;; => 6
  ((within 10) 16)
  ;; => 6.0
  ((within 10) -6)
  ;; => 4.0
  ((within 10) -16))
;; => 4.0

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

(comment
  (v- [0 1] [1 2])
  ;; => [-1 -1]
  ())

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

(defn random-asteroid []
  (let [height (second play-area)
        min-speed 1/2
        max-speed 2
        full-rotation (* 2 Math/PI)
        max-omega (/ full-rotation 40)
        min-start (/ height 4)
        max-start (/ height 2)]
    {:position (rectangular (rand-bounded min-start max-start) (rand-angle))
     :velocity (rectangular (rand-bounded min-speed max-speed) (rand-angle))
     :angle (rand-angle)
     :angular-velocity (rand-bounded (- max-omega) max-omega)
     :mass 3}))

(comment
  (random-asteroid))

(defn spawn-asteroid
  ([state] (spawn-asteroid state {}))
  ([state asteroid]
   (update state :asteroids conj (merge (random-asteroid) asteroid))))

(def initial-state
  {:controls #{}
   :events (conj queue
                 :respawn
                 :spawn-asteroid
                 :spawn-asteroid
                 :spawn-asteroid
                 :spawn-asteroid)

   :lasers []

   :asteroids []
   })

(defn shoot [{:keys [ship] :as state}]
  (if (< (count (:lasers state)) 4)
    (let [fresh-laser {:position (v+ (:position ship)
                                     (rectangular ship-radius (:angle ship)))
                       :velocity (rectangular laser-speed (:angle ship))
                       :countdown 30}]
      (update state :lasers conj fresh-laser))
    state))

(defn setup []
  (q/frame-rate 30)
  initial-state)

(defn detect-collisions
  "Enqueue events/transform state based on collisions in current state"
  [state]
  (let [ship-asteroid-collisions :TODO
        laser-asteroid-collisions :TODO]
    (-> state
        ;; ship + asteroid: ship respawns
        (update :events conj :respawn)
        ;; laser + asteroid: laser dies, smaller asteroids spawn/smallest die
        )))

(comment
  (shoot initial-state)
  {:controls #{}
   ;; :events #object[clojure.lang.PersistentQueue 0x38326fb9 "clojure.lang.PersistentQueue@48a66cc8"]
   :ship {:angle 0
          :position [320 240]
          :velocity [0 0]
          :ammo 4}
   :lasers [{:position [330.0 240.0]
             :velocity [10.0 0.0]}]})

(defn effects
  "functions from state to state"
  [game-event]
  (get
   {:shoot shoot
    :respawn spawn-ship
    :spawn-asteroid spawn-asteroid}
   game-event
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

(defn move-lasers [state]
  (update state :lasers #(into []
                               (comp
                                (map move-laser)
                                (filter (fn [laser] (< 0 (:countdown laser)))))
                               %)))

(defn move-asteroid [{:keys [velocity angular-velocity] :as asteroid}]
  (-> asteroid
      (update :position #(on-game-torus (v+ % velocity)))
      (update :angle #(+ % angular-velocity))))

(defn move-asteroids [state]
  (update state :asteroids #(map move-asteroid %)))

(defn move-objects [state]
  (-> state
      move-asteroids
      move-lasers
      (update :ship move-ship)
      ))

(defn update-state [state]
  (-> state
      ;; detect-collisions
      process-events
      actuate-controls
      move-objects
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
  [p]
  (let [[width height] play-area]
    (into #{}
          (comp
           (map #(v+ p %))
           (filter (fn [[x y]]
                     (and
                      (<= (- ship-radius) x) (< x (+ width ship-radius))
                      (<= (- ship-radius) y) (< y (+ height ship-radius))))))
          torus-translations
          )))

(comment
  (ship-torus-positions [9 9])
  ;; => #{[9 9] [649 489] [9 489] [649 9]}
  (ship-torus-positions [10 10])
  ;; => #{[10 10]}
  (ship-torus-positions [1 1])
  ;; => #{[1 1] [641 1] [1 481] [641 481]}
  (ship-torus-positions (mapv #(* 1/2 %) play-area))
  ;; => #{[320N 240N]}
  ())

(defn draw-ship [{:keys [position angle thrusting?]}]
  (q/stroke 200)
  (doseq [p (ship-torus-positions position)]
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
        (when thrusting?
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

(comment
  (laser-torus-positions {:position [1 2]
                          :velocity [5 5]})
  #{[1 2] [641 482]})

(defn draw-laser [{:keys [position velocity] :as laser}]
  (q/stroke 255)
  (doseq [p (laser-torus-positions laser)]
    (q/with-translation p
      (q/line [0 0] (mapv - velocity)))))

;; (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) 9))
;; => (0 0.6981317007977318 1.3962634015954636 2.0943951023931953 2.792526803190927 3.490658503988659 4.1887902047863905 4.886921905584122 5.585053606381854)

(defn asteroid-torus-positions [asteroid]
  (let [[width height] play-area
        major-radius (* 10 (:mass asteroid))]
    (into #{}
          (filter (fn [[x y]]
                    (and
                     (<= (- major-radius) x) (< x (+ width major-radius))
                     (<= (- major-radius) y) (< y (+ height major-radius)))))
          (torus-points (:position asteroid)))))

(comment
  (asteroid-torus-positions {:position [0 0] :mass 3})
  ;; => #{[0 0] [640 0] [640 480] [0 480]}
  (asteroid-torus-positions {:position [320 0] :mass 3})
  ;; => #{[320 0] [320 480]}
  (asteroid-torus-positions {:position [320 2] :mass 3})
  ;; => #{[320 482] [320 2]}
  (asteroid-torus-positions {:position [2 240] :mass 3})
  ;; => #{[642 240] [2 240]}
  (asteroid-torus-positions {:position [632 240] :mass 3})
  ;; => #{[-8 240] [632 240]}
  ())

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
  (doseq [laser lasers] (draw-laser laser))
  (doseq [asteroid asteroids] (draw-asteroid asteroid)))

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
