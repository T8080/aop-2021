(ns day-25)

(def input (->> (slurp "day-25.txt")
                clojure.string/split-lines vec))

(def height (count input))
(def width (count (first input)))

(defn normalize [[y x]]
  [(rem (+ y height) height)
   (rem (+ x width) width)])

(defn grid-map [grid f]
  (vec (map-indexed (fn [y row]
                      (vec (map-indexed (fn [x cell] (f y x cell))
                                        row)))
                    grid)))

(defn move-right [grid]
  (grid-map grid
            (fn [y x cell]
              (cond (and (= cell \.) (= (get-in grid (normalize [y (dec x)])) \>)) \>
                    (and (= cell \>) (= (get-in grid (normalize [y (inc x)])) \.)) \.
                    :else cell))))

(defn move-down [grid]
  (grid-map grid
            (fn [y x cell]
              (cond (and (= cell \.) (= (get-in grid (normalize [(dec y) x])) \v)) \v
                    (and (= cell \v) (= (get-in grid (normalize [(inc y) x])) \.)) \.
                    :else cell))))

(defn step [grid]
  (-> grid
      move-right
      move-down))

(defn fixpoint-depth [f x i]
  (let [next (f x)]
    (if (= x next) i
      (fixpoint-depth f next (inc i)))))

(fixpoint-depth step input 1)
