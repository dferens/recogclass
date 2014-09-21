(ns recogclass.lab1.build
  (:import java.awt.Color
           java.awt.BasicStroke
           java.awt.geom.Ellipse2D
           org.jfree.chart.annotations.XYLineAnnotation
           org.jfree.chart.annotations.XYShapeAnnotation)
  (:require [clojure.edn :as edn]
            [incanter.core]
            [incanter.charts]
            [recogclass.lab1.cancer :refer [cancer]]
            [recogclass.lab1.spectr :refer [spectr]]
            [recogclass.lab1.trout :refer [trout] :reload true]
            [recogclass.lab1.utils :as utils :reload true]))

;;
;; Dataset transformations
;;

(defn load-dataset
  [dataset-file-path]
  (edn/read-string (slurp dataset-file-path)))

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
    [id-a id-b (utils/calc-decart-distance vec-a vec-b)]))

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
;; Results builders
;;

(defn- save-chart
  [chart file]
  (incanter.core/save chart file :width 1280 :height 720))

(defn- annotate-scatter-chart-with-names!
  [chart property-matrix]
  (let [reducer (fn [chart [id x y]]
                  (incanter.charts/add-pointer chart x y :text id :angle :se))]
    (reduce reducer chart property-matrix)))

(defn- annotate-chart-with-edges!
  [chart edges property-matrix]
  (let [stroke (new BasicStroke 2.0)
        paint (new Color (rand-int 256) (rand-int 256) (rand-int 256))
        reducer (fn [chart [fac-id-1 fac-id-2]]
                  (let [[x1 y1] (utils/get-record-properties property-matrix fac-id-1)
                        [x2 y2] (utils/get-record-properties property-matrix fac-id-2)
                        annotation (new XYLineAnnotation x1 y1 x2 y2 stroke paint)]
                    (.addAnnotation (.getXYPlot chart) annotation)
                    chart))]
    (reduce reducer chart edges)))

(defn- annotate-chart-with-circles!
  [chart circles]
  (doseq [{center :center radius :radius} circles]
    (let [height (* radius 2) width (* radius 2)
          top-left-x (- (first center) radius)
          top-left-y (- (second center) radius)
          shape (java.awt.geom.Ellipse2D$Double. top-left-x top-left-y width height)
          stroke (new BasicStroke 2.0)
          outline-paint Color/BLUE
          circle (new XYShapeAnnotation shape stroke outline-paint)]
      (.addAnnotation (.getXYPlot chart) circle)))
  chart)

(defn build-initial
  [property-matrix]
  (save-chart
   (-> (incanter.charts/scatter-plot
        (map second property-matrix)
        (map #(get % 2) property-matrix)
        :title "Початкові дані"
        :x-label "Відсоток з усіма атестаціями"
        :y-label "Відсоток з 3ма неатестаціями")
       (annotate-scatter-chart-with-names! property-matrix))
   "target/scatter.png"))

(defn build-spectr
  [distance-map]
  (let [spectr-result (spectr distance-map)]
    (save-chart
     (incanter.charts/line-chart
      (map first spectr-result)
      (map second spectr-result)
      :title "Результати алгоритму СПЕКТР"
      :x-label "Факультети"
      :y-label "Значення спектра")
     "target/spectr.png")))

(defn build-cancer
  [property-matrix distance-graph]
  (let [[cancer-groups cancer-edges functionals] (cancer distance-graph)
        groups-count (count cancer-groups)]
    (save-chart
      (let [x-serie (map second property-matrix)
            y-serie (map #(get % 2) property-matrix)
            group-by-serie (map #(utils/get-group-number cancer-groups (first %)) property-matrix)]
        (-> (incanter.charts/scatter-plot x-serie y-serie
             :title (format "Результати алгоритму КРАБ (кількість груп: %d)" groups-count)
             :x-label "Відсоток з усіма атестаціями"
             :y-label "Відсоток з 3ма неатестаціями"
             :group-by group-by-serie)
            (annotate-scatter-chart-with-names! property-matrix)
            (annotate-chart-with-edges! cancer-edges property-matrix)))
      "target/cancer.png")
    (save-chart
      (let [x-serie (range (count functionals))
            y-serie (map :val functionals)
            categories [:D :R :G :H]
            chart (incanter.charts/line-chart
                   x-serie y-serie
                   :legend true
                   :title "Функціонали"
                   :x-label "N"
                   :y-label "Значення функціоналу")]
        (doseq [category categories]
          (incanter.charts/add-categories chart
           x-serie (map category functionals)
           :series-label (str (name category) " компонента")))
        chart)
      "target/cancer-functionals.png")))

(defn build-trout
  [property-matrix]
  (let [{:keys [initial-sphere spheres-built]} (trout property-matrix)
        circles (map :sphere spheres-built)
        groups (map :separates spheres-built)
        groups-count (count groups)
        group-by-serie (map #(utils/get-group-number groups (first %)) property-matrix)]
    (save-chart
     (-> (incanter.charts/scatter-plot
          (map second property-matrix)
          (map #(get % 2) property-matrix)
          :title (format "Результати алгоритму ФОРЕЛЬ (кількість груп: %d)" groups-count)
          :x-label "Відсоток з усіма атестаціями"
          :y-label "Відсоток з 3ма неатестаціями"
          :group-by group-by-serie)
         (annotate-scatter-chart-with-names! property-matrix)
         (annotate-chart-with-circles! circles))
     "target/trout.png")))

(defn build-dataset [dataset]
  (let [property-matrix (dataset->property-matrix dataset)
        distance-matrix (property-matrix->distance-matrix property-matrix)
        distance-map (distance-matrix->distance-map distance-matrix)
        distance-graph (distance-map->distance-graph distance-map)]

    (build-initial property-matrix)
    (build-spectr distance-map)
    (build-cancer property-matrix distance-graph)
    (build-trout property-matrix)))

(def dataset (load-dataset "resources/lab1/faculties.edn"))

(build-dataset dataset)
