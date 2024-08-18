(ns quilsteroids.geometry)

(defn within
  "Function which clips the value passed to at least zero and at most the given width"
  [width]
  (fn [x]
    (let [whole-widths (Math/floor (/ x width))]
      (- x (* width whole-widths)))
    ))

(defn within? [[b & bounds] [c & coordinates]]
  (if (nil? b)
      true
      (and (let [[lo hi] b]
             (<= lo c hi))
           (recur bounds coordinates))))

(comment
  (within? [[0 640] [0 480]] [320 240])
  ;; => true
  (within? [[0 640] [0 480]] [820 240])
  ;; => false
  ())

(defn on-torus
  "Function which clips the point passed to its argument to the given width and height"
  [[width height]]
  (let [clip-width (within width)
        clip-height (within height)]
    (fn [[x y]]
      [(clip-width x) (clip-height y)])))

(defn rectangular [r theta]
  [(* r (Math/cos theta)) (* r (Math/sin theta))])

(def v+ (partial mapv +))
(def v- (partial mapv -))

(defn rand-angle []
  (* (rand) 2 Math/PI))

(comment
  (rand-angle))

(defn rand-bounded [least most]
  (+ least (* (rand) (- most least))))

(comment
  (rand-bounded 1/2 2))

(defn norm-squared [v]
  (reduce + (map * v v)))

(defn distance-squared
  "distance squared between p1 and p2"
  [p1 p2]
  (norm-squared (map - p1 p2)))

(defprotocol Geometry
  (on [this position])
  (positions [this position]))

(defn torus [width height]
  (let [translations
        [[(- width) (- height)] [0 (- height)] [width (- height)]
         [(- width)    0      ] [0    0      ] [width    0      ]
         [(- width)    height ] [0    height ] [width    height ]]

        on* (on-torus [width height])]
    (reify Geometry
      (on [this position]
        (on* position))
      (positions [this position]
        (map #(v+ position %) translations)))))

