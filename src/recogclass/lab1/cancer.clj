(ns recogclass.lab1.cancer
  (:require [loom.alg]
            [loom.graph]
            [recogclass.lab1.utils :as utils]))

;;
;; Functionals parts
;;

(defn- calc-functional-D
  [loom-graph]
  1)

(defn- calc-functional-H
  [loom-graph]
  1)

(defn- calc-functional-G
  [loom-graph]
  1)


(defn- calc-functional-R
  [loom-graph]
  (let [groups (->> (loom.alg/connected-components loom-graph)
                    (map #(apply hash-set %)))]
    (* (/ 1 (count groups))
       (apply + (for [group groups
                      :let [elements-count (count group)]
                      :when (> elements-count 1)]
                    (* (/ 1 elements-count)
                       (->> (utils/get-group-edges loom-graph group)
                            (map #(loom.graph/weight loom-graph %))
                            (apply +))))))))

;;
;; Main graph functional
;;

(defn calc-functional
  [loom-graph]
  (let [D (calc-functional-D loom-graph)
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
           functionals []]
      (let [max-edge (utils/get-max-edge loom-graph)
            new-loom-graph (loom.graph/remove-edges loom-graph max-edge)
            old-functional-val (last functionals)
            new-functional-val (calc-functional new-loom-graph)]
        (if (or (nil? old-functional-val)
                (> new-functional-val old-functional-val))
          (recur new-loom-graph (conj functionals new-functional-val))
          [(map set (loom.alg/connected-components loom-graph))
           (loom.graph/edges loom-graph)
           functionals])))))
