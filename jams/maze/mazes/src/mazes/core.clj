(ns mazes.core)

(require '[clojure.data.json :as json])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn empty-vec
  "Generates an empty vector of n size"
  [size init]
  (vec (repeat size init)))

(defn cell
  "Returns the cell value"
  [grid [x y]]
  ((grid y) x))

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
    (> y y1) (update-in (update-in grid [y x] + 1) [y1 x1] + 2)
))

(defn random-cell
  "pick a random cell in the grid and return its position as x and y"
  [grid carved]
  (let [ height (count grid)
         width (count (first grid))]
    (if (empty? carved)
      [(rand-int width), (rand-int height)]
      (rand-nth carved))))

(defn carve-grid
  "Carve a single wall out of a grid"
  [{:keys [grid carved]}]
  (let [current-cell (random-cell grid carved)
        neighbor-to-carve (uncarved-neighbor grid current-cell)]
    (if (nil? neighbor-to-carve)
      {:grid grid :carved (remove #{current-cell} carved)}
      {:grid (carve grid current-cell neighbor-to-carve) :carved (conj carved neighbor-to-carve)})))

(defn generate
  "Generate a grid of specified size and carve out a maze"
  [width height strategy]
  ;(json/write-str (empty-array height (empty-array width 0))))
  (let [grid (empty-vec height (empty-vec width 0))
        carved []]
    (first
      (drop-while #(not (empty? (:carved %)))
        (rest
          (iterate carve-grid {:grid grid :carved carved}))))))
