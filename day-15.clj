(ns day-15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def costs (mapv (fn [line] (mapv #(Character/getNumericValue %) line))
                 (str/split-lines (slurp "day-15.txt"))))

(def height (count costs))
(def width (count (first costs)))

(defn cost [y x]
  (let [extra (+ (quot y height) (quot x width))
        base (get-in costs [(rem y height) (rem x width)])]
    (inc (rem (+ base extra -1) 9))))

(def min-cost
  (memoize (fn [y x]
             (cond (and (= y 0) (= x 0)) 0
                   (= y 0) (+ (cost y x) (min-cost y (dec x)))
                   (= x 0) (+ (cost y x) (min-cost (dec y) x))
                   :else   (+ (cost y x) (min (min-cost y (dec x))
                                              (min-cost (dec y) x)))))))

