(ns recogclass.lab1.spectr)


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
