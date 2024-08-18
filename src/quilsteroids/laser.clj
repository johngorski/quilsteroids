(ns quilsteroids.laser
  (:require
   [quil.core :as q]
   [quilsteroids.geometry :as geometry :refer [v+ v-]]
   [quilsteroids.kitchen-sink :refer [filter-values map-values]]
   [quilsteroids.object :as object]
   ))

(def laser-speed 10)

(defn exhaust-laser
  [state laser-id]
  (object/remove-object state :laser laser-id))

(defn visible? [laser {:keys [when-at within]}]
  (let [p when-at
        [[x-min x-max] [y-min y-max]] within
        inside? (fn [point]
                  (geometry/within? [[x-min x-max]
                                     [y-min y-max]]
                                    point))]
    (or (inside? p)
        (inside? (v- p (:velocity laser))))))

(defn move-laser [laser geo]
  (-> laser
      (update :position #(geometry/on geo (v+ % (:velocity laser))))
      (update :countdown dec)))

(defn move-lasers [state]
  (update state :lasers #(into {}
                               (comp
                                (map-values move-laser)
                                (filter-values (fn [laser] (< 0 (:countdown laser)))))
                               %)))

(defn draw-laser [{:keys [velocity] :as laser} position]
  (q/stroke 255)
  (let [p position]
    (q/with-translation p
      (q/line [0 0] (mapv - velocity)))))

(defrecord Laser [position velocity countdown]
  object/Thing
  (draw [this position]
    (draw-laser this position))

  (position [this]
    position)

  (move [this geo]
    (move-laser this geo))

  (visible? [this under-circumstances]
    (visible? this under-circumstances)))

(defn fresh-laser [{:keys [position angle]}]
  (map->Laser {:position position
               :velocity (geometry/rectangular laser-speed angle)
               :countdown 30}))



