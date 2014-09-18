(ns recogclass.lab1
  (:require [clojure.edn :as edn]))

(def dataset
  (edn/read-string (slurp "resources/lab1/faculties.edn")))
