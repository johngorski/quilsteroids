(ns quilsteroids.core
  (:require
   [clojure.set :as sets]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import (clojure.lang PersistentQueue)))

(defn setup []
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;; setup function returns initial state. It contains
  {:angle 0
   :controls #{}
   :events PersistentQueue/EMPTY})

(defn update-state [{:keys [controls] :as state}]
  (let [turn-rate (/ Math/PI 20)]
    (assoc state :angle (+ (:angle state)
                           (* (if (:left-turn controls) -1 0) turn-rate)
                           (* (if (:right-turn controls) 1 0) turn-rate)))))

(defn vec2d+ [a b]
  (mapv + a b))

(defn rectangular [r theta]
  [(* r (Math/cos theta)) (* r (Math/sin theta))])

(defn draw-ship [{:keys [x y theta thrusting?]}]
  (let [R 10
        r 7
        delta (* 5/6 Math/PI)
        left-angle (+ theta delta)
        right-angle (- theta delta)
        pos [x y]
        nose (vec2d+ pos (rectangular R theta))
        left-tip (vec2d+ pos (rectangular R left-angle))
        left-base (vec2d+ pos (rectangular r left-angle))
        right-tip (vec2d+ pos (rectangular R right-angle))
        right-base (vec2d+ pos (rectangular r right-angle))
        tail (vec2d+ pos (rectangular R (+ theta Math/PI)))]
    (q/stroke 200)
    (q/line nose left-tip)
    (q/line nose right-tip)
    (q/line left-base right-base)
    (when thrusting?
      (q/line left-base tail)
      (q/line right-base tail))))

(defn draw-state [{:keys [angle controls] :as state}]
  (q/background 0)
  ;; Move origin point to the center of the sketch.
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (draw-ship {:x 0 :y 0 :theta angle :thrusting? (:thrusters controls)})))

(def held-controls
  {:up :thrusters
   :left :left-turn
   :right :right-turn})

(def triggers
  {:control :fire
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
    :title "Quilsteroids!"
    :size [500 500]
    ;; setup function called only once, during sketch initialization.
    :setup setup
    ;; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state

    :key-pressed key-pressed
    :key-released key-released

    :features [:keep-on-top]
    ;; This sketch uses functional-mode middleware.
    ;; Check quil wiki for more info about middlewares and particularly
    ;; fun-mode.
    :middleware [m/fun-mode]))
