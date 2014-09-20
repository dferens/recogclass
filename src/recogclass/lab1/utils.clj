(ns recogclass.lab1.utils
  (:require [clojure.set :as set]
            [loom.graph]))


(defn calc-decart-distance
  "Calculates Decart distance of two vectors"
  [vec-a vec-b]
  (->> (map #(Math/pow (- %1 %2) 2) vec-a vec-b)
       (reduce +)
       (Math/sqrt)))

(defn get-max-edge
  "Returns edge with max weight"
  [loom-graph]
  (->> (loom.graph/edges loom-graph)
       (sort-by #(loom.graph/weight loom-graph %))
       (last)))

(defn get-group-number
  "Accepts collection of sets (groups) and value, returns group index
   which contains given value"
  [groups value]
  (loop [group-index 0]
    (let [group (nth groups group-index)]
      (when (not (nil? group))
        (if (contains? group value)
          group-index
          (recur (inc group-index)))))))

(defn get-group-edges
  [loom-graph group]
  (->> (loom.graph/edges loom-graph)
       (filter #(not (empty? (set/intersection (apply hash-set %) group))))))
