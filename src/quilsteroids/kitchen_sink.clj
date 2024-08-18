(ns quilsteroids.kitchen-sink
  (:import (clojure.lang PersistentQueue)))

(def queue PersistentQueue/EMPTY)

(defn map-values
  "Applies f to every value in m, leaving keys unchanged. Returns a transducer when m is not provided."
  ([f]
   (map (fn [[k v]] [k (f v)])))
  ([f m]
   (into {} (map-values f) m)))

(defn filter-values
  "Filters m according to the truthiness of (f v) for the value v of each entry in m.
  Returns a transducer when m is not provided."
  ([f]
   (filter (fn [[k v]] (f v))))
  ([f m]
   (into {} (filter-values f) m)))

