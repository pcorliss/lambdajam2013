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

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
