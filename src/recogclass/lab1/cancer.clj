(ns recogclass.lab1.cancer
  (:require [clojure.set :as set]
            [loom.alg]
            [loom.graph]
            [recogclass.lab1.utils :as utils]))


;;
;; Functionals parts
;;

(defn- calc-functional-D
  [initial-graph removed-edges]
  (/ (->> removed-edges
          (map #(loom.graph/weight initial-graph %))
          (apply +))
     (count removed-edges)))

(defn- calc-functional-H
  [mst-graph]
  (let [total-vertex-count (count (loom.graph/nodes mst-graph))
        groups (utils/get-graph-groups mst-graph)
        groups-count (count groups)
        average-vertexes-per-group (Math/ceil (/ total-vertex-count groups-count))]
    (if (= groups-count 1)
      (throw (ex-info "Can not calculate H component for graph with 1 group" {}))
      (apply * (for [group groups
                     :let [group-vertex-count (count group)
                           ratio (/ group-vertex-count total-vertex-count)]]
                 (->> (double (/ group-vertex-count total-vertex-count))
                      (- 1)
                      (Math/abs)
                      (- 1)))))))

(defn- get-min-group-neigh
  "Returns smallest edge which is neighbor to current edge
  and connects vertexes of same group.
  Can return nil of group has no edges within."
  [current-edge mst-graph removed-edges]
  (let [current-edge (set current-edge)
        removed-edges (set (map set removed-edges))]
    (->> (loom.alg/distinct-edges mst-graph)
         (map set)
         ; Get direct edges-neighbors
         (filter #(not (empty? (set/intersection % current-edge))))
         ; Get edges which are in some group (simply not in removed edges)
         (filter #(not (contains? removed-edges %)))
         (map vec)
         (sort-by #(loom.graph/weight mst-graph %))
         (first))))



(defn- calc-functional-G
  [initial-graph mst-graph removed-edges]
  (/ (apply + (for [edge removed-edges
                    :let [min-neigh (get-min-group-neigh edge mst-graph removed-edges)]
                    ; Some vertexes may not have connected edges
                    :when (not (nil? min-neigh))]
                (/ (loom.graph/weight mst-graph min-neigh)
                   (loom.graph/weight initial-graph edge))))
     (count removed-edges)))


(defn- calc-functional-R
  [mst-graph]
  (let [groups (utils/get-graph-groups mst-graph)]
    (/ (apply + (for [group groups
                      :let [elements-count (count group)]
                      :when (> elements-count 1)]
                    (/ (->> (utils/get-group-edges mst-graph group)
                            (map #(loom.graph/weight mst-graph %))
                            (apply +))
                       elements-count)))
       (count groups))))

;;
;; Main graph functional
;;

(defn calc-functional
  [initial-graph mst-graph new-removed-edges]
  (let [D (calc-functional-D initial-graph new-removed-edges)
        H (calc-functional-H mst-graph)
        G (calc-functional-G initial-graph mst-graph new-removed-edges)
        R (calc-functional-R mst-graph)]
    {:D D :H H :G G :R R
     :val (Math/log (/ (* D H) G R))}))

(defn cancer
  "Splits graph vectors to groups and returns three element vector:
   [({:one} {:two :tree} ...) - collection of sets of vectors
    ([:one :two] [:one :three] ...) - collection of graph edges left
    (0.987 0.876 0.321 ...) - collection of functionals]"
  [distance-graph]
  (let [initial-loom-graph (apply loom.graph/weighted-graph distance-graph)]
    (loop [mst-graph (loom.alg/prim-mst initial-loom-graph)
           removed-edges []
           functionals []]
      (let [max-edge (utils/get-max-edge mst-graph)
            new-mst-graph (loom.graph/remove-edges mst-graph max-edge)
            new-removed-edges (conj removed-edges max-edge)
            old-functional (last functionals)
            new-functional (calc-functional initial-loom-graph
                                            new-mst-graph
                                            new-removed-edges)
            new-functionals (conj functionals new-functional)]
        (if (or (nil? old-functional)
                (> (new-functional :val) (old-functional :val)))
          (recur
             new-mst-graph
             new-removed-edges
             new-functionals)
          [(map set (loom.alg/connected-components mst-graph))
           (loom.graph/edges mst-graph)
           new-functionals])))))
