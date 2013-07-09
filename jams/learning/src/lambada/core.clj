(ns lambada.core
  "Contributors:
   Gary Fredericks
   Denny Abraham
   Joseph Jackson
   Michael Harrison
   Darren Ng
   Devin Walters
   Philip Corliss"
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn read-data
  "Read in data"
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
      (csv/read-csv in-file :separator \,))))

(defn parse-data
  "Parse the data"
  [filename]
  (for [line (rest (read-data filename))]
    (for [element line]
      (Long/parseLong element))))

(def training-set (parse-data "digitssample.csv"))

(defn diff-squared
  "Finds the difference between numbers and squares it"
  [n m]
  (* (- n m) (- n m)))

(defn compare-grid
  "Compares two number grids and returns the RMS"
  [grid1 grid2]
  (Math/sqrt (/ (apply + (map diff-squared grid1 grid2)) 784)))

(defn classifier
  "Classifies using RMS between all samples"
  [unknown-grid]
  (first
   (apply min-key #(compare-grid unknown-grid (rest %)) training-set)))

(defn classify-all
  "Read in CSV and classify all numbers"
  []
  (let [check-set (parse-data "digitscheck.csv")]
    (pmap #(= (first %) (classifier (rest %))) check-set)))
