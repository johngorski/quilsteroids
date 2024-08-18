(ns quilsteroids.asteroid
  (:require
   [quil.core :as q]
   [quilsteroids.geometry :as geometry :refer [v+]]
   [quilsteroids.kitchen-sink :refer [map-values]]
   [quilsteroids.object :as object]))

(def min-asteroid-speed 1/2)
(def max-asteroid-speed 2)

(defn rand-asteroid-velocity
  ""
  []
  (geometry/rectangular (geometry/rand-bounded min-asteroid-speed max-asteroid-speed)
                        (geometry/rand-angle)))

(defn move-asteroid [{:keys [velocity angular-velocity] :as asteroid} geo]
  (-> asteroid
      (update :position #(geometry/on geo (v+ % velocity)))
      (update :angle #(+ % angular-velocity))))

(defn move-asteroids [state]
  (update state :asteroids #(map-values move-asteroid %)))

(defn asteroid-radius [asteroid]
  (* 10 (:mass asteroid)))

(defn visible? [asteroid {:keys [when-at within]}]
  (let [[x y] when-at
        [[x-min width] [y-min height]] within
        major-radius (asteroid-radius asteroid)]
    (and
     (<= (- x-min major-radius) x) (< x (+ width major-radius))
     (<= (- y-min major-radius) y) (< y (+ height major-radius)))))

;; TODO: Will be refactored to separate geometry and visibility functions
#_(defn asteroid-torus-positions [asteroid]
  (let [[width height] play-area
        major-radius (asteroid-radius asteroid)]
    (into #{}
          (filter (fn [[x y]]
                    (and
                     (<= (- major-radius) x) (< x (+ width major-radius))
                     (<= (- major-radius) y) (< y (+ height major-radius)))))
          (torus-points (:position asteroid)))))

(defn draw-asteroid [{:keys [position angle mass] :as asteroid} p]
  (q/stroke 180)
  (let [segments 9
        major-radius (* 10 mass)
        minor-radius (* 10 (dec mass))
        arc-angle (/ (* 2 Math/PI) segments)
        angles (range 0 (* 2 Math/PI) arc-angle)
        [inner & outers] angles]
    ;; (doseq [p (asteroid-torus-positions asteroid)]) ;; now being passed directly
    (q/with-translation p
      (q/with-rotation [angle]
        (doseq [[t1 t2] (partition 2 1 outers)]
          (q/line (geometry/rectangular major-radius t1) (geometry/rectangular major-radius t2)))
        (let [inner-p (geometry/rectangular minor-radius inner)]
          (q/line inner-p (geometry/rectangular major-radius (first outers)))
          (q/line inner-p (geometry/rectangular major-radius (last outers))))))))

(defrecord Asteroid [position velocity angle angular-velocity mass]
  object/Thing
  (draw [this position]
    (draw-asteroid this position))

  (position [this]
    position)

  (move [this geo]
    (move-asteroid this geo))

  (visible? [this under-circumstances]
    (def *dbg* {:this this :under-circumstances under-circumstances})
    (visible? this under-circumstances)))

(defn random-asteroid
  "Random asteroid, optionally with presets."
  ([presets] (merge (random-asteroid) presets))
  ([]
   (let [min-speed 1/2
         max-speed 2
         full-rotation (* 2 Math/PI)
         max-omega (/ full-rotation 40)]
     (map->Asteroid {:position [0 0]
                     :velocity (rand-asteroid-velocity)
                     :angle (geometry/rand-angle)
                     :angular-velocity (geometry/rand-bounded (- max-omega) max-omega)
                     :mass 3}))))

(comment
  (random-asteroid)
  (random-asteroid {:position [13 37] :mass 1}))

(defn spawn-asteroid
  ([state] (spawn-asteroid state {}))
  ([state asteroid]
   (object/add-object state :asteroid (merge (random-asteroid) asteroid))))

(defn spawn-asteroid-at [position]
  (fn [state]
    (spawn-asteroid state (random-asteroid {:position position}))))

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
    (let [without-asteroid (object/remove-object state :asteroid asteroid-id)]
      (reduce (fn [state chunk] (object/add-object state :asteroid chunk))
              without-asteroid
              (smaller-asteroids asteroid)))
    state))

