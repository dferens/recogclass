(ns recogclass.lab1.trout
  (:require [recogclass.lab1.utils :as utils]))


(defprotocol SphereProtocol
  (is-point-inside [this point])
  (rand-point-inside [this])
  (get-points-inside [this points-lookup])
  (contains-points? [this points-lookup]))

(defrecord Sphere [center radius]
  SphereProtocol
  (is-point-inside
   [this point]
   (<= (utils/calc-decart-distance point center)
       radius))
  (rand-point-inside
   [this]
   (let [angle (-> (rand) (* 360) (Math/toRadians))
         distance (rand radius)]
    [(+ (first center) (* (Math/cos angle) distance))
     (+ (second center) (* (Math/sin angle) distance))]))
  (get-points-inside
    [this points-lookup]
    (let [points-inside-keys
          (for [key (keys points-lookup)
                :let [point (get points-lookup key)]
                :when (is-point-inside this point)]
            key)]
      (select-keys points-lookup points-inside-keys)))
  (contains-points?
    [this points-lookup]
    (some #(is-point-inside this %) (vals points-lookup))))

(defn- property-matrix->points-lookup
  "Returns map of format:
  {:point-one (1.23 4.56) :point-two ... }"
  [property-matrix]
  (reduce
   (fn [result record]
     (assoc result (first record) (rest record)))
   (hash-map)
   property-matrix))

(defn- calc-sphere-radius
  [center points-coords]
  (loop [current-point-index 0
         current-max-radius nil]
    (let [current-point (nth points-coords current-point-index nil)]
      (if (nil? current-point)
        current-max-radius
        (let [new-radius (utils/calc-decart-distance center current-point)]
          (if (or (nil? current-max-radius)
                  (> new-radius current-max-radius))
            (recur (inc current-point-index) new-radius)
            (recur (inc current-point-index) current-max-radius)))))))

(defn calc-points-mass-center
  "Calculates mass center of points collection"
  [points-lookup]
  (let [coords (vals points-lookup)
        axis-count (count (first coords))]
    (for [axis-index (range axis-count)
          :let [axis-value-getter #(nth % axis-index)]]
      (/ (apply + (map axis-value-getter coords))
         (count coords)))))

(defn- get-most-far-point
  "Returns coords of most far point from `from-point` and distance to it."
  ([points-lookup]
   (get-most-far-point points-lookup (first (vals points-lookup))))
  ([points-lookup from-point]
   (reduce (fn [[most-far-point most-far-distance] next-point]
             (let [new-distance (utils/calc-decart-distance from-point next-point)]
               (if (or (nil? most-far-point)
                       (> new-distance most-far-distance))
                 [next-point new-distance]
                 [most-far-point most-far-distance])))
           [nil nil]
           (vals points-lookup))))

(defn get-sphere-of-points
  "Returns minimal sphere which contains all given points."
  [points-lookup]
  (if (= (count points-lookup) 1)
    (->Sphere (first (vals points-lookup)) 1)
    (let [[point-a _] (get-most-far-point points-lookup)
          [point-b _] (get-most-far-point points-lookup point-a)
          center (map #(+ %1 (/ (- %2 %1) 2)) point-a point-b)
          radius (/ (utils/calc-decart-distance point-a point-b) 2)]
      (->Sphere center radius))))

(defn- get-final-trout-sphere
  "Returns final sphere and points it contains"
  [start-sphere points-lookup]
  (loop [current-sphere start-sphere]
    (let [current-points-inside-lookup (get-points-inside current-sphere points-lookup)
          new-sphere-center (calc-points-mass-center current-points-inside-lookup)
          new-sphere (->Sphere new-sphere-center (:radius current-sphere))
          new-points-inside-lookup (get-points-inside new-sphere points-lookup)]
      (if (empty? new-points-inside-lookup)
        [current-sphere current-points-inside-lookup]
        (if (= (-> current-points-inside-lookup keys set)
               (-> new-points-inside-lookup keys set))
          [new-sphere new-points-inside-lookup]
          (recur new-sphere))))))

(defn- get-start-trout-sphere
  "Returns random sphere inside current one with given radius which has
  any points inside."
  [current-sphere points-lookup radius]
  (loop []
    (let [new-sphere-center (rand-point-inside current-sphere)
          new-sphere (->Sphere new-sphere-center radius)]
      (if (contains-points? new-sphere points-lookup)
        new-sphere
        (recur)))))

(defn trout
  "Splits input objects into groups using trout algorithm.
  Returns results map:
  {:initial-sphere - initial sphere which contains all objects
   :spheres-built - collection of produced spheres:
    {:sphere - actual `Sphere` record, map with :center and :radius keys
     :separates - set of object ids which given sphere contains}"
  [property-matrix radius-ratio]
  (let [points-lookup (property-matrix->points-lookup property-matrix)
        initial-sphere (get-sphere-of-points points-lookup)
        search-sphere-radius (* radius-ratio (:radius initial-sphere))]
    (loop [spheres-built []
           points-left-lookup points-lookup]
      (if (empty? points-left-lookup)
        {:initial-sphere initial-sphere
         :spheres-built spheres-built}
        (let [current-start-sphere (get-sphere-of-points points-left-lookup)
              start-sphere (get-start-trout-sphere
                             current-start-sphere
                             points-left-lookup
                             search-sphere-radius)
              [final-sphere final-points-inside-lookup]
              (get-final-trout-sphere start-sphere points-left-lookup)]
          (recur
           (conj spheres-built {:sphere final-sphere
                                :separates (set (keys final-points-inside-lookup))})
           (apply dissoc points-left-lookup (keys final-points-inside-lookup))))))))



