(ns quilsteroids.ship
  (:require
   [quil.core :as q]
   [quilsteroids.geometry :as geometry :refer [v+]]
   [quilsteroids.laser :as laser]
   [quilsteroids.object :as object]
   ))

(def ship-radius 10)
(def turn-rate (/ Math/PI 20))
(def ship-acceleration 1/4)

(defn move-ship [ship geo]
  (update ship :position #(geometry/on geo (v+ % (:velocity ship)))))

(defn visible? [ship {:keys [when-at within]}]
  (let [[x y] when-at
        [[x-min width] [y-min height]] within]
    (and
     (<= (- x-min ship-radius) x) (< x (+ width ship-radius))
     (<= (- y-min ship-radius) y) (< y (+ height ship-radius)))))

(defn draw-ship [{:keys [angle] :as ship} p]
  (q/stroke 200)
  (q/with-translation p
    (let [R ship-radius
          r (* 7/10 R)
          delta (* 5/6 Math/PI)
          left-angle (+ angle delta)
          right-angle (- angle delta)
          nose (geometry/rectangular R angle)
          left-tip (geometry/rectangular R left-angle)
          left-base (geometry/rectangular r left-angle)
          right-tip (geometry/rectangular R right-angle)
          right-base (geometry/rectangular r right-angle)
          tail (geometry/rectangular R (+ angle Math/PI))]
      (q/line nose left-tip)
      (q/line nose right-tip)
      (q/line left-base right-base)
      (when (:thrusting? ship)
        (q/line left-base tail)
        (q/line right-base tail)))))

(defrecord Ship [angle position velocity ammo]
  object/Thing
  (draw [this position]
    (draw-ship this position))

  (position [this]
    position)

  (move [this geo]
    (move-ship this geo))

  (visible? [this under-circumstances]
    (visible? this under-circumstances)))


(defn ship-at [position]
  (map->Ship {:angle 0
              :position position
              :velocity [0 0]
              :ammo 4}))

(defn spawn-ship-at [position]
  (fn [state]
    (assoc state :ship (ship-at position))))

(defn fresh-laser [^Ship ship]
  (laser/fresh-laser {:position (v+ (:position ship)
                                    (geometry/rectangular ship-radius (:angle ship)))
                      :angle (:angle ship)}))

;; TODO: Replace game/shoot with this function
(defn shoot [ship]
  (fn [state]
    (if (< (count (:lasers state)) 4)
      (object/add-object state :laser (fresh-laser ship))
      state)))

