(ns recogclass.lab1
  (:require [clojure.edn :as edn]
            [incanter.core :as incanter]
            [incanter.charts :as charts]))

(defn calc-decart-distance
  "Calculates Decart distance"
  [vec-a vec-b]
  (->> (map #(Math/pow (- %1 %2) 2) vec-a vec-b)
       (reduce +)
       (Math/sqrt)))

(def dataset
  (edn/read-string (slurp "resources/lab1/faculties.edn")))

(defn make-object-property-matrix
  "Returns list of vectors (table) with 3 elements:
     * faculty id
     * full atestations %
     * no 3 atestations %"
  [dataset]
  (for [faculty-id (keys (dataset :has-full-atestations))]
    [faculty-id
     (get-in dataset [:has-full-atestations faculty-id])
     (get-in dataset [:has-no-three-atestations faculty-id])]))


(defn make-distance-matrix
  "Returns matrix square(N)*3 where N is number of rows in object-property table"
  [object-property-matrix]
  (for [record-a object-property-matrix
        record-b object-property-matrix
        :let [id-a (first record-a) vec-a (rest record-a)
              id-b (first record-b) vec-b (rest record-b)]]
    [id-a id-b (calc-decart-distance vec-a vec-b)]))

(defn make-distance-map
  "Converts distance matrix to map like:
  {faculty1-id
     {faculty1-id 0.123
      faculty2-id ...}
   ...}"

  [distance-matrix]
  (reduce (fn [result record]
            (assoc-in result [(first record) (second record)] (last record)))
          {}
          distance-matrix))

;;
;; Spectr algorithm
;;

(defn calc-spectr-value
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


(def object-property-matrix
  (make-object-property-matrix dataset))

(def spectr-result
  (-> object-property-matrix
      make-distance-matrix
      make-distance-map


      spectr))

(first object-property-matrix)


(defn build []
  ;;
  ;; Initial scatter
  ;;
  (incanter/save
    (charts/scatter-plot
     (map #(get % 1) object-property-matrix)
     (map #(get % 2) object-property-matrix)
     :x-label "Відсоток з усіма атестаціями"
     :y-label "Відсоток з 3ма неатестаціями")
   "scatter.png")

  ;;
  ;; Spectr algo results
  ;;
  (incanter/save
  (charts/line-chart
    (map first spectr-result)
    (map second spectr-result)
    :x-label "Факультети"
    :y-label "Значення спектра")
   "spectr.png"))


(build)

