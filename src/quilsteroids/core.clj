(ns quilsteroids.core
  (:require
   [clojure.set :as sets]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import (clojure.lang PersistentQueue)))

(defn setup []
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  {:controls #{}
   :events PersistentQueue/EMPTY

   :ship
   {:angle 0
    :position [0 0]
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

(defn actuate-controls [{:keys [controls ship] :as state}]
  (-> state
      (update-in [:ship :angle]
                 #(+ %
                     (* turn-rate
                        (+ (if (:left-turn controls) -1 0)
                           (if (:right-turn controls) 1 0)))))
      (update-in [:ship :velocity]
                 #(v+ % (rectangular
                         (if (:thrusters controls) 1/4 0)
                         (:angle ship))))))

(defn move-ship [ship]
  (update ship :position #(v+ % (:velocity ship))))

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

(defn draw-ship [{:keys [position angle thrusting?]}]
  (let [R 10
        r 7
        delta (* 5/6 Math/PI)
        left-angle (+ angle delta)
        right-angle (- angle delta)
        nose (v+ position (rectangular R angle))
        left-tip (v+ position (rectangular R left-angle))
        left-base (v+ position (rectangular r left-angle))
        right-tip (v+ position (rectangular R right-angle))
        right-base (v+ position (rectangular r right-angle))
        tail (v+ position (rectangular R (+ angle Math/PI)))]
    (q/stroke 200)
    (q/line nose left-tip)
    (q/line nose right-tip)
    (q/line left-base right-base)
    (when thrusting?
      (q/line left-base tail)
      (q/line right-base tail))))

(defn draw-state [{:keys [controls ship] :as state}]
  (q/background 0)
  ;; Move origin point to the center of the sketch.
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (draw-ship (assoc ship :thrusting? (:thrusters controls)))))

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
    :size [640 480]
    ;; setup function called only once, during sketch initialization.
    :setup setup
    ;; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :key-pressed key-pressed
    :key-released key-released
    ))
