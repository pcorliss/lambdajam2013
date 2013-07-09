(ns lambada.core)

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn read-data
  "Read in data"
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
      (csv/read-csv in-file :separator \,))))

(defn parse-data
  "Parse the data"
  []
  (for [line (rest (read-data "digitscheck.csv"))]
    (for [element line]
      (Long/parseLong element))))

(defn diff-squared
  "Finds the difference between numbers and squares it"
  [n m]
  (* (- n m) (- n m)))

(defn compare-grid
  "Compares two number grids and returns the RMS"
  [grid1 grid2]
  (. Math sqrt (/ (apply + (map diff-squared grid1 grid2)) 784)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
