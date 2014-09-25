(ns recogclass.lab1.build
  (:import java.awt.Color
           java.awt.BasicStroke
           java.awt.geom.Ellipse2D
           org.jfree.chart.annotations.XYLineAnnotation
           org.jfree.chart.annotations.XYShapeAnnotation)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [incanter.core]
            [incanter.charts]
            [incanter.stats]
            [recogclass.lab1.cancer :refer [cancer]]
            [recogclass.lab1.spectr :refer [spectr]]
            [recogclass.lab1.trout :refer [trout] :reload true]
            [recogclass.lab1.prims :refer [prims] :reload true]
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
  [property-matrix metric-func]
  (for [record-a property-matrix
        record-b property-matrix
        :let [id-a (first record-a) vec-a (rest record-a)
              id-b (first record-b) vec-b (rest record-b)]]
    [id-a id-b (metric-func vec-a vec-b)]))

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

(defn- annotate-chart-with-names!
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
  [chart circles color]
  (doseq [{center :center radius :radius} circles]
    (let [height (* radius 2) width (* radius 2)
          top-left-x (- (first center) radius)
          top-left-y (- (second center) radius)
          shape (java.awt.geom.Ellipse2D$Double. top-left-x top-left-y width height)
          stroke (new BasicStroke 2.0)
          circle (new XYShapeAnnotation shape stroke color)]
      (.addAnnotation (.getXYPlot chart) circle)))
  chart)

(defn get-x-y-series
  [property-matrix]
  [(map second property-matrix)
   (map #(nth % 2) property-matrix)])

(defn build-initial
  [property-matrix]
  (save-chart
   (-> (incanter.charts/scatter-plot
        (map second property-matrix)
        (map #(get % 2) property-matrix)
        :title "Початкові дані"
        :x-label "Відсоток з усіма атестаціями"
        :y-label "Відсоток з 3ма неатестаціями")
       (annotate-chart-with-names! property-matrix))
   "target/scatter.png"))

(defn build-spectr
  [distance-map output-dir]
  (let [spectr-result (spectr distance-map)]
    (save-chart
     (incanter.charts/line-chart
      (map first spectr-result)
      (map second spectr-result)
      :title "Результати алгоритму СПЕКТР"
      :x-label "Факультети"
      :y-label "Значення спектра")
     (str output-dir "spectr.png"))))

(defn build-cancer
  [property-matrix distance-graph output-dir]
  (let [[cancer-groups cancer-edges functionals] (cancer distance-graph)
        groups-count (count cancer-groups)]
    (save-chart
      (let [[x-serie y-serie] (get-x-y-series property-matrix)
            group-by-serie (map #(utils/get-group-number cancer-groups (first %)) property-matrix)]
        (-> (incanter.charts/scatter-plot x-serie y-serie
             :title (format "Результати алгоритму КРАБ (кількість груп: %d)" groups-count)
             :x-label "Відсоток з усіма атестаціями"
             :y-label "Відсоток з 3ма неатестаціями"
             :group-by group-by-serie)
            (annotate-chart-with-names! property-matrix)
            (annotate-chart-with-edges! cancer-edges property-matrix)))
      (str output-dir "cancer.png"))
    (save-chart
      (let [x-serie (range (count functionals))
            y-serie (map :val functionals)
            categories [:D :R :G :H]
            chart (incanter.charts/line-chart x-serie y-serie
                   :legend true
                   :title "Функціонали"
                   :x-label "N"
                   :y-label "Значення функціоналу")]
        (doseq [category categories]
          (incanter.charts/add-categories chart
           x-serie (map category functionals)
           :series-label (str (name category) " компонента")))
        chart)
      (str output-dir "cancer-functionals.png"))))

(defn build-trout
  [property-matrix output-dir]
  (doseq [ratio-string ["0.9" "0.6" "0.4" "0.25"]]
    (let [ratio (Float/parseFloat ratio-string)
          {:keys [initial-sphere spheres-built]} (trout property-matrix ratio)
          circles (map :sphere spheres-built)
          groups (map :separates spheres-built)
          group-by-serie (map #(utils/get-group-number groups (first %)) property-matrix)
          [x-serie y-serie] (get-x-y-series property-matrix)]
      (save-chart
       (-> (incanter.charts/scatter-plot x-serie y-serie
            :title (format "Результати алгоритму ФОРЕЛЬ (кількість груп: %d, ratio: %f)"
                           (count groups) ratio)
            :x-label "Відсоток з усіма атестаціями"
            :y-label "Відсоток з 3ма неатестаціями"
            :group-by group-by-serie)
           (annotate-chart-with-names! property-matrix)
           (annotate-chart-with-circles! [initial-sphere] Color/RED)
           (annotate-chart-with-circles! circles Color/BLUE))
       (format "%strout-%s.png" output-dir ratio-string)))))

(defn build-prims
  [property-matrix distance-graph output-dir]
  (let [[x-serie y-serie] (get-x-y-series property-matrix)
        mst-edges (loom.graph/edges (prims distance-graph))
        _ (prn mst-edges)]
    (save-chart
      (-> (incanter.charts/scatter-plot x-serie y-serie
           :title "Результати алгоритму Пріма"
           :x-label "Відсоток з усіма атестаціями"
           :y-label "Відсоток з 3ма неатестаціями")
          (annotate-chart-with-names! property-matrix)
          (annotate-chart-with-edges! mst-edges property-matrix))
      (str output-dir "prims.png"))))

(def metrics
  [{:name "decart" :func incanter.stats/euclidean-distance}
   {:name "cosine" :func (comp #(- 1 %) incanter.stats/cosine-similarity)}])

(defn build-dataset [dataset]
  (let [property-matrix (dataset->property-matrix dataset)]
    (build-initial property-matrix)
    (for [metric metrics]
      (let [dir (format "target/%s/" (metric :name))
            distance-matrix (property-matrix->distance-matrix property-matrix (metric :func))
            distance-map (distance-matrix->distance-map distance-matrix)
            distance-graph (distance-map->distance-graph distance-map)]
        (.mkdir (io/file dir))
        (prn distance-matrix)
        (build-spectr distance-map dir)
        (build-cancer property-matrix distance-graph dir)
        (build-trout property-matrix dir)
        (build-prims property-matrix distance-graph dir)))))

(def dataset (load-dataset "resources/lab1/faculties.edn"))

(build-dataset dataset)
