(ns day-13 (:require [clojure.string :as str]
                     [clojure.set :as set]))

(defn parse-point [str]
  (mapv #(Integer/parseInt %)
        (str/split str #",")))

(defn parse-instruction [instr]
  (let [[_ axis-str n-str] (re-find #"fold along (\w)=(\d+)" instr)
        axis (keyword axis-str)
        n (Integer/parseInt n-str)]
    [axis n]))

(def input-raw
  (str/split (slurp "day-13.txt") #"\n\n"))

(def points
  (->> input-raw first str/split-lines (map parse-point) set))

(def instructions
  (->> input-raw second str/split-lines (map parse-instruction)))

(defn fold-point [[axis n] point]
  (let [x (if (= axis :x) (first point) (second point))
        x* (+ n (- n x))]
    (if (= axis :x) [x* (second point)] [(first point) x*])))

(defn fold [points [axis n]]
  (let [axis-getter (if (= axis :x) first second)
        foldees (filter #(> (axis-getter %) n) points)
        folded (map #(fold-point [axis n] %) foldees)]
    (set/union (set/difference points (set foldees))
               (set folded))))

(defn display-points [points]
  (let [width (inc (apply max (map first points)))
        height (inc (apply max (map second points)))
        board (reduce (fn [board [x y]] (assoc board (+ x (* y width)) \#))
                      (vec (repeat (* width height) " "))
                      points)]
    (doseq [row (partition width board)]
      (println (str/join "" row)))))

(count (fold points (first instructions)))
(display-points (reduce fold points instructions))
