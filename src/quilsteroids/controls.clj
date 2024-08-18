(ns quilsteroids.controls)

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



