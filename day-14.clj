(ns day-14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> "day-14.txt" slurp (str/split #"\n\n")))

(def init (first input))

(def rules (->> input second str/split-lines (map parse-rule) (into {})))

(defn parse-rule [rule]
  (let [[lh rh] (str/split rule #" -> ")]
    [(vec lh) (first rh)]))

(def vfreqs (frequencies init))

(def pfreqs
  (let [pairs (partition 2 1 init)]
    (into {} (map (fn [pair] [pair (count (filter #(= % pair) pairs))])
                  (keys rules)))))

(defn delta [pair freq]
  (let [new (rules pair)
        pair1 [(first pair) new]
        pair2 [new (second pair)]
        dvfreqs {new freq}
        dpfreqs (merge-with + {pair (- freq)} {pair1 freq} {pair2 freq})]
    [dpfreqs dvfreqs]))

(defn step [[pfreqs vfreqs]]
  (reduce (fn [[pd vd] pair]
            (let [[pd* vd*] (delta pair (pfreqs pair))
                  pd (merge-with + pd* pd)
                  vd (merge-with + vd* vd)]
              [pd vd]))
          [pfreqs vfreqs]
          (keys rules)))

(defn code [[pfreqs vfreqs]]
  (let [freqs (vals vfreqs)]
    (- (apply max freqs) (apply min freqs))))

(code (iter step [pfreqs vfreqs] 40))
