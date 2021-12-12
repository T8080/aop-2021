(ns day-12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def connections (->> (slurp "day-12.txt")
                      str/split-lines
                      (mapv #(str/split % (re-pattern "-")))))

(def nodes (-> connections flatten set))

(defn find-neighbours [node]
  (->> connections
       (map (fn [[a b]] (cond (= a node) b (= b node) a)))
       (filter #(not= % nil))
       (set)))

(def neighbours
  (zipmap nodes (map find-neighbours nodes)))

(defn small? [node]
  (= node (str/lower-case node)))

(defn paths [from to nodes special path]
  (let [nodes (if (and (small? from) (not= special from)) (disj nodes from) nodes)
        special (if (= special from) nil special)
        nexts (set/intersection nodes (neighbours from))
        path (conj path from)]
    (cond (= from to) (list (reverse path))
          (empty? nexts) '(())
          :else (mapcat (fn [n] (paths n to nodes special path))
                     nexts))))

(def double-nodes (filter #(and (small? %) (not= "start" %) (not= "end" %)) nodes))
(def possible-paths (set (mapcat #(paths "start" "end" nodes % '()) double-nodes)))
(dec (count possible-paths))
