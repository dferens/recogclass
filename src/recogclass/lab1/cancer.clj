(ns recogclass.lab1.cancer
  (:require loom.alg loom.graph
            [recogclass.lab1.utils :as utils]))

;;
;; Functionals parts
;;

(defn- calc-functional-D
  [loom-graph]
  (rand-int 10))

(defn- calc-functional-H
  [loom-graph]
  1)

(defn- calc-functional-G
  [loom-graph]
  1)

(defn- calc-functional-R
  [loom-graph]
  1)

(defn calc-functional
  [loom-graph]
  (Math/log (* (calc-functional-D loom-graph)
               (calc-functional-H loom-graph)
               (/ 1 (calc-functional-G loom-graph))
               (/ 1 (calc-functional-R loom-graph)))))

(defn cancer
  "Splits graph vectors to groups and returns two element vector:
   [({:one} {:two :tree} ...) - collection of sets of vectors
    ([:one :two] [:one :three] ...) - collection of graph edges left]"
  [distance-graph]
  (let [initial-loom-graph (apply loom.graph/weighted-graph distance-graph)
        loom-mst-graph (loom.alg/prim-mst initial-loom-graph)]
    (loop [loom-graph loom-mst-graph
           old-functional-val -99999]
      (let [max-edge (utils/get-max-edge loom-graph)
            new-loom-graph (loom.graph/remove-edges loom-graph max-edge)
            new-functional-val (calc-functional new-loom-graph)]
        (if (> new-functional-val old-functional-val)
          (recur new-loom-graph new-functional-val)
          (identity
           [(->> loom-graph
                 (loom.alg/connected-components)
                 (map set))
            (loom.graph/edges loom-graph)]))))))
