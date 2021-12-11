(defn map2d [f board]
  (mapv #(mapv f %) board))

(def board (->> (slurp "day-11.txt")
                clojure.string/split-lines
                (map2d #(Character/getNumericValue %))))

(def height (count board))
(def width (count (first board)))

(defn neighbours [[y x]]
  (for [yy (range (- y 1) (+ y 2))
        xx (range (- x 1) (+ x 2))
        :when (and (not (and (= x xx) (= y yy)))
                   (<= 0 xx (dec width))
                   (<= 0 yy (dec height)))]
    [yy xx]))


(defn flash [board queue]
  (def coord (first queue))
  (def value (get-in board coord 0))
  (cond (empty? queue) board,
        (= value -1) (flash board (rest queue)),
        (>= value 9) (flash (assoc-in board coord -1)
                            (concat queue (neighbours coord)))
        :else (flash (assoc-in board coord (inc value))
                     (rest queue))))

(defn simulate [board i n flash-count]
  (def board-inc (map2d inc board))
  (def flash-coords
    (for [y (range height)
          x (range width)
          :when (> (get-in board-inc [y x]) 9)]
      [y x]))
  (def flashed-board (flash board-inc flash-coords))
  (def new-flash-count (->> flashed-board
                            flatten
                            (filter #(= % -1))
                            count))
  (when (= new-flash-count (* width height)) (println (inc i)))
  (def next-board (map2d #(if (< % 0) 0 %) flashed-board))
  (if (= i n) flash-count
      (simulate next-board (inc i) n (+ flash-count new-flash-count))))

(simulate board 0 100 0)
