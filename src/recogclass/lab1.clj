(ns recogclass.lab1
  (:require [clojure.edn :as edn]
            [incanter.core]
            [incanter.charts]
            [incanter.datasets]
            [loom.io]
            [loom.alg]
            [loom.graph]))

;;
;; Utilities
;;

(defn calc-decart-distance
  "Calculates Decart distance of two vectors"
  [vec-a vec-b]
  (->> (map #(Math/pow (- %1 %2) 2) vec-a vec-b)
       (reduce +)
       (Math/sqrt)))

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

(defn dataset->property-matrix
  "Returns list of vectors (table) with 3 elements:
     * faculty id
     * full atestations %
     * no 3 atestations %"
  [dataset]
  (for [faculty-id (keys (dataset :has-full-atestations))]
    [faculty-id
     (get-in dataset [:has-full-atestations faculty-id])
     (get-in dataset [:has-no-three-atestations faculty-id])]))

(defn property-matrix->distance-matrix
  "Returns matrix [N*N x 3]:
  [[faculty1-id faculty1-id 0.000]
   [faculty1-id faculty2-id 0.123]
   [faculty2-id faculty1-id 0.456]
    ...]"
  [property-matrix]
  (for [record-a property-matrix
        record-b property-matrix
        :let [id-a (first record-a) vec-a (rest record-a)
              id-b (first record-b) vec-b (rest record-b)]]
    [id-a id-b (calc-decart-distance vec-a vec-b)]))

(defn distance-matrix->distance-map
  "Converts distance matrix to map like:
  {faculty1-id
     {faculty1-id 0.000
      faculty2-id 0.123}
   ...}"

  [distance-matrix]
  (reduce (fn [result record]
            (assoc-in result (take 2 record) (last record)))
          {}
          distance-matrix))

(defn distance-map->distance-graph
  "Returns graph as map of edges:
  [[:edge1 :edge2 0.123]
   [:edge2 :edge3 0.456] ... ]"
  [distance-map]
  (let [elements (keys distance-map)]
    (for [start-index (range (count elements))
          iter-element (drop (inc start-index) elements)
          :let [start-element (nth elements start-index)
                distance (get-in distance-map [start-element iter-element])]]
      [start-element iter-element distance])))

;;
;; Spectr algorithm
;;

(defn- calc-spectr-value
  [element b-seq distance-map]
  (* (/ 1 (count b-seq))
     (apply + (for [b-element b-seq]
                (get-in distance-map [element b-element])))))
(defn spectr
  [distance-map]
  (let [elements (keys distance-map)]
    (loop [elements (rest elements)
           b-seq [(first elements)]
           spectr-seq []]
      (if (empty? elements)
        (map vector b-seq spectr-seq)
        (let [func #(hash-map :id % :val (calc-spectr-value % b-seq distance-map))
              closest-element (last (sort-by :val (map func elements)))]
          (recur
           (remove #{(closest-element :id)} elements)
           (conj b-seq (closest-element :id))
           (conj spectr-seq (closest-element :val))))))))

;;
;; Cancer algoritm (ffs ....)
;;

(defn cancer-functional-D
  [loom-graph]
  (rand-int 10))

(defn cancer-functional-H
  [loom-graph]
  1)

(defn cancer-functional-G
  [loom-graph]
  1)

(defn cancer-functional-R
  [loom-graph]
  1)

(defn cancer-functional
  [loom-graph]
  (Math/log (* (cancer-functional-D loom-graph)
               (cancer-functional-H loom-graph)
               (/ 1 (cancer-functional-G loom-graph))
               (/ 1 (cancer-functional-R loom-graph)))))

(defn get-max-edge
  [loom-graph]
  (->> (loom.graph/edges loom-graph)
       (sort-by #(loom.graph/weight loom-graph %))
       (last)))

(defn cancer
  [distance-graph]
  (let [initial-loom-graph (apply loom.graph/weighted-graph distance-graph)
        loom-mst-graph (loom.alg/prim-mst initial-loom-graph)]
    (loop [loom-graph loom-mst-graph
           old-functional-val -99999]
      (let [max-edge (get-max-edge loom-graph)
            new-loom-graph (loom.graph/remove-edges loom-graph max-edge)
            new-functional-val (cancer-functional new-loom-graph)]
        (if (> new-functional-val old-functional-val)
          (recur new-loom-graph new-functional-val)
          (->> loom-graph
               (loom.alg/connected-components)
               (map set)))))))

;;
;; Results builders
;;

(defn build-initial
  [property-matrix]
  (incanter.core/save
   (incanter.charts/scatter-plot
    (map second property-matrix)
    (map #(get % 2) property-matrix)
    :title "Початкові дані"
    :x-label "Відсоток з усіма атестаціями"
    :y-label "Відсоток з 3ма неатестаціями")
   "target/scatter.png"))

(defn build-spectr
  [distance-map]
  (let [spectr-result (spectr distance-map)]
    (incanter.core/save
     (incanter.charts/line-chart
      (map first spectr-result)
      (map second spectr-result)
      :title "Результати алгоритму СПЕКТР"
      :x-label "Факультети"
      :y-label "Значення спектра")
     "target/spectr.png")))

(defn build-cancer
  [property-matrix distance-graph]
  (let [cancer-result (cancer distance-graph)
        groups-count (count cancer-result)]
    (incanter.core/save
     (let [x-serie (map second property-matrix)
           y-serie (map #(get % 2) property-matrix)]
       (incanter.charts/scatter-plot x-serie y-serie
        :title (format "Результати алгоритму КРАБ (%d групи)" groups-count)
        :x-label "Відсоток з усіма атестаціями"
        :y-label "Відсоток з 3ма неатестаціями"
        :group-by (map #(get-group-number cancer-result (first %)) property-matrix)))
     "target/cancer.png")))

(defn build [dataset]
  (let [property-matrix (dataset->property-matrix dataset)
        distance-matrix (property-matrix->distance-matrix property-matrix)
        distance-map (distance-matrix->distance-map distance-matrix)
        distance-graph (distance-map->distance-graph distance-map)]

    (build-initial property-matrix)
    (build-spectr distance-map)
    (build-cancer property-matrix distance-graph)))

(def dataset
  (edn/read-string (slurp "resources/lab1/faculties.edn")))

(build dataset)
