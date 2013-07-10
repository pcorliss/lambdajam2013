(ns mazes.core)

(require '[clojure.data.json :as json])

(defn ascii-cell-str
  "returns appropriate string for an ascii cell"
  [value]
  (str
    (if (bit-test value 1)
      " "
      "_")
    (if (bit-test value 2)
      " "
      "|")))

(defn print-ascii-maze
  "Prints out an ascii appropriate maze"
  [grid]
  (print (apply str
    (str " " (apply str (repeat (count (first grid)) "_ ")) "\n")
    (map #(str "|" (apply str (map ascii-cell-str %)) "\n" ) grid))))

(defn empty-vec
  "Generates an empty vector of n size"
  [size init]
  (vec (repeat size init)))

(defn random-cell
  "pick a random cell in the grid and return its position as x and y"
  [grid carved]
  (let [ height (count grid)
         width (count (first grid))]
    (if (empty? carved)
      [(rand-int width), (rand-int height)]
      (rand-nth carved))))

(defn north
  "Returns the position for a cell north of the specified cell"
  [grid [x y]]
  (if (>= y 1)
    [x (- y 1)]))

(defn south
  "Returns the position for a cell south of the specified cell"
  [grid [x y]]
  (if (< y (- (count grid) 1))
    [x (+ y 1)]))

(defn west
  "Returns the position for a cell west of the specified cell"
  [grid [x y]]
  (if (>= x 1)
    [(- x 1) y]))

(defn east
  "Returns the position for a cell east of the specified cell"
  [grid [x y]]
  (if (< x (- (count (first grid)) 1))
    [(+ x 1) y]))

(defn cell
  "Returns the cell value"
  [grid [x y]]
  ((grid y) x))

(defn uncarved-neighbor
  "Given a grid and x, y position, return the position of a random neighbor which has not been carved"
  [grid [x y]]
  (let [neighbors [(north grid [x y]) (south grid [x y]) (west grid [x y]) (east grid [x y])]
        eligible (filter #(= 0 (cell grid %)) (remove nil? neighbors))]
    (if (not (empty? eligible))
      (rand-nth eligible))))

(defn carve
  "Given a cell position, and it's neighbor's cell position, carve out the wall between then return the grid"
  [grid [x y] [x1 y1]]
  (cond
    (< x x1) (update-in (update-in grid [y x] + 4) [y1 x1] + 8)
    (> x x1) (update-in (update-in grid [y x] + 8) [y1 x1] + 4)
    (< y y1) (update-in (update-in grid [y x] + 2) [y1 x1] + 1)
    (> y y1) (update-in (update-in grid [y x] + 1) [y1 x1] + 2)))

(def RESET "&#092;&#048;33[0m")

(defn print-grid
  [{:keys [grid carved]}]
  (print (format "%s" "\33[2J"))
  (print-ascii-maze grid)
  (flush)
  (Thread/sleep 10)
  {:grid grid :carved carved}
)

(defn carve-grid
  "Carve a single wall out of a grid"
  [{:keys [grid carved]}]
  (let [current-cell (random-cell grid carved)
        neighbor-to-carve (uncarved-neighbor grid current-cell)]
    (if (nil? neighbor-to-carve)
      {:grid grid :carved (remove #{current-cell} carved)}
      (print-grid {:grid (carve grid current-cell neighbor-to-carve) :carved (conj carved neighbor-to-carve)}))))

(defn generate
  "Generate a grid of specified size and carve out a maze"
  [width height strategy]
  (let [grid (empty-vec height (empty-vec width 0))
        carved [(random-cell grid [])]]
    (first
      (drop-while #(not (empty? (:carved %)))
        (iterate carve-grid {:grid grid :carved carved})))))

(defn print-json-maze
  "Prints out a JSON compatible maze"
  [grid]
  (println (json/write-str grid)))

(defn print-maze
  ""
  [width height print-type]
  (let [grid (:grid (generate width height :default))]
    (cond
      (= print-type :json) (print-json-maze grid)
      (= print-type :ascii) (print-ascii-maze grid))))

(defn -main
  [& args]
  (print-maze 19 19 :none))
