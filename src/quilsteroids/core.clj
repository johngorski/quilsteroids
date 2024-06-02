(ns quilsteroids.core
  (:require
   [clojure.set :as sets]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import (clojure.lang PersistentQueue)))

;; TODOs (refactoring throughout, tests overdue)
;; - lasers
;; - asteroids
;; - collisions
;; - cap ship speed

(def play-area
  [640 480])

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

(defn setup []
  (q/frame-rate 30)
  {:controls #{}
   :events PersistentQueue/EMPTY

   :ship
   {:angle 0
    :position (mapv #(/ % 2) play-area)
    :velocity [0 0]
    :ammo 4}
   })

(def v+ (partial mapv +))

(defn detect-collisions
  "Enqueue events/transform state based on collisions in current state"
  [state]
  identity ;; TODO
  )

(defn process-events
  "Process event queue to update the state"
  [state]
  identity ;; TODO
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

(defn move-objects [state]
  (update state :ship move-ship))

(defn update-state [state]
  (-> state
      ;; detect-collisions
      ;; process-events
      actuate-controls
      move-objects
      ))

(defn rectangular [r theta]
  [(* r (Math/cos theta)) (* r (Math/sin theta))])

(def ship-radius 10)

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
          [[(- width) (- height)] [0 (- height)] [width (- height)]
           [(- width)    0      ] [0    0      ] [width    0      ]
           [(- width)    height ] [0    height ] [width    height ]]
          )))

(comment
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

(defn draw-state [{:keys [controls ship] :as state}]
  (q/background 0)
  (draw-ship (assoc ship :thrusting? (:thrusters controls))))

(def held-controls
  {:up :thrusters
   :left :left-turn
   :right :right-turn})

(def triggers
  {:control :shoot
   :down :warp})

(defn key-pressed [old-state event]
  (if-let [held-control (held-controls (:key event))]
    (update old-state :controls conj held-control)
    old-state))

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
