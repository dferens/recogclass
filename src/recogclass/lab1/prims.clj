(ns recogclass.lab1.prims
  (:require [loom.alg]
            [loom.graph]))

(defn prims
  [distance-graph]
  (loom.alg/prim-mst (apply loom.graph/weighted-graph distance-graph)))
