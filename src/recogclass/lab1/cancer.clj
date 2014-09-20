(ns recogclass.lab1.cancer
  (:require [loom.alg]
            [loom.graph]
            [recogclass.lab1.utils :as utils]))


;;
;; Functionals parts
;;

(defn- calc-functional-D
  [loom-graph removed-edges]
  (/ (->> removed-edges
          (map #(loom.graph/weight loom-graph %))
          (apply +))
     (count removed-edges)))

(defn- calc-functional-H
  [loom-graph]
  1)

(defn- calc-functional-G
  [loom-graph]
  1)


(defn- calc-functional-R
  [loom-graph]
  (let [groups (utils/get-graph-groups loom-graph)]
    (/ (apply + (for [group groups
                      :let [elements-count (count group)]
                      :when (> elements-count 1)]
                    (/ (->> (utils/get-group-edges loom-graph group)
                            (map #(loom.graph/weight loom-graph %))
                            (apply +))
                       elements-count)))
       (count groups))))

;;
;; Main graph functional
;;

(defn calc-functional
  [loom-graph new-removed-edges]
  (let [D (calc-functional-D loom-graph new-removed-edges)
        H (calc-functional-H loom-graph)
        G (calc-functional-G loom-graph)
        R (calc-functional-R loom-graph)]
    (Math/log (* D H (/ 1 G) (/ 1 R)))))

(defn cancer
  "Splits graph vectors to groups and returns three element vector:
   [({:one} {:two :tree} ...) - collection of sets of vectors
    ([:one :two] [:one :three] ...) - collection of graph edges left
    (0.987 0.876 0.321 ...) - collection of functionals]"
  [distance-graph]
  (let [initial-loom-graph (apply loom.graph/weighted-graph distance-graph)
        loom-mst-graph (loom.alg/prim-mst initial-loom-graph)]
    (loop [loom-graph loom-mst-graph
           removed-edges []
           functionals []]
      (let [max-edge (utils/get-max-edge loom-graph)
            new-loom-graph (loom.graph/remove-edges loom-graph max-edge)
            new-removed-edges (conj removed-edges max-edge)
            old-functional-val (last functionals)
            new-functional-val (calc-functional initial-loom-graph new-removed-edges)]
        (if (or (nil? old-functional-val)
                (> new-functional-val old-functional-val))
          (recur
             new-loom-graph
             new-removed-edges
             (conj functionals new-functional-val))
          [(map set (loom.alg/connected-components loom-graph))
           (loom.graph/edges loom-graph)
           (conj functionals new-functional-val)])))))
