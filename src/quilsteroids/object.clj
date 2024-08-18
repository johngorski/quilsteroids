(ns quilsteroids.object
  (:require
   [quilsteroids.geometry :as geometry]
   ))

(defprotocol Thing
  (move [this geometry]
    "The updated Thing after an undisturbed timestep with respect to the given geometry")
  (position [this] "Where the Thing currently is")
  ;; (angle [this] "Rotation of this thing")
  (visible? [this {:keys [when-at within]}]
    "Whether this Thing is visble when-at the provided location within the width and height provided.")
  (draw [this position]
    "Draw the Thing at the given position"))

(def object-type-keys {:asteroid :asteroids, :laser :lasers})

(defn add-object
  "Add an obj of type type (:laser / :asteroid) to the game state."
  [state type obj]
  (update state (object-type-keys type) assoc (random-uuid) obj))

(defn remove-object
  "Removes the object with the given id of type type from the game state."
  [state type id]
  (update state (object-type-keys type) dissoc id))

(defn visible-positions [object {:keys [geo viewport]}]
  (into #{}
        (filter #(visible? object {:when-at %
                                   :within viewport}))
        (geometry/positions geo (position object))))

