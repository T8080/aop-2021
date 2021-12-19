(ns day-17)

(def s "target area: x=102..157, y=-146..-90")

(def bounds [102 157 -146 -90])

(def x-min (nth bounds 0))
(def x-max (nth bounds 1))
(def y-min (nth bounds 2))
(def y-max (nth bounds 3))

(defn naturals-sum [x]
  (/ (* x (+ x 1)) 2))

(naturals-sum (dec (Math/abs (min y-min y-max))))
