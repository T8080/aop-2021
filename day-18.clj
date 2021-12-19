(ns day-18)

(defn parse [content tokens]
  (case (first tokens)
    nil (first content)
    \] [content (rest tokens)]
    \[ (let [[n-content n-tokens] (parse [] (rest tokens))]
         (parse (conj content n-content) n-tokens))
    \, (parse content (rest tokens))
    (parse (conj content (Character/getNumericValue (first tokens))) (rest tokens))))

(defn extend-path [number path dir]
  (let [node (get-in number path)]
    (cond (or (nil? path) (nil? node)) nil
          (number? node) path
          (= dir :left) (extend-path number (conj path 0) dir)
          (= dir :right) (extend-path number (conj path (dec (count node))) dir))))

(defn left-branch [path]
  (cond (empty? path) nil
        (= 0 (last path)) (left-branch (pop path))
        :else (update path (dec (count path)) dec)))

(defn right-branch [number path]
  (cond (empty? path) nil
        (>= (inc (last path)) (count (get-in number (pop path)))) (right-branch number (pop path))
        :else (update path (dec (count path)) inc)))

(defn explode [number path]
  (let [l (get-in number (conj path 0))
        r (get-in number (conj path 1))
        l-path (extend-path number (left-branch path) :right)
        r-path (extend-path number (right-branch number path) :left)
        n (assoc-in number path 0)
        n (if l-path (update-in n l-path #(+ l %)) n)
        n (if r-path (update-in n r-path #(+ r %)) n)]
    n))

(defn explode-path [number path]
  (let [node (get-in number path)]
    (cond (= 4 (count path)) (and node (coll? node) path)
          (not (coll? node)) nil
          :else (reduce #(if %1 %1 %2)
                        (map #(explode-path number (conj path %))
                             (range (count node)))))))

(defn split [number path]
  (let [val (get-in number path)
        l (quot val 2)
        r (quot (inc val) 2)]
    (assoc-in number path [l r])))

(defn split-path [number path]
  (let [node (get-in number path)]
    (cond (and (number? node) (>= node 10)) path
          (not (coll? node)) nil
          :else (reduce #(if %1 %1 %2)
                        (map #(split-path number (conj path %))
                             (range (count node)))))))

(defn step [number]
  (if-let [path (explode-path number [])]
    (explode number path)
    (if-let [path (split-path number [])]
      (split number path)
      number)))

(defn fixpoint [f x]
  (if (= x (f x)) x
      (fixpoint f (f x))))

(defn snail-add [n1 n2]
  (fixpoint step [n1 n2]))

(defn magnitude [number]
  (if (number? number) number
      (+ (* 3 (magnitude (first number)))
         (* 2 (magnitude (second number))))))

(defn pairs [xs]
  (let [ps (for [x xs, y xs :when (not= x y)] [x y])
        rs (map reverse ps)]
    (lazy-cat ps rs)))

(def numbers (->> (slurp "day-18.txt")
                  (clojure.string/split-lines)
                  (map #(parse [] %1))))

(magnitude (reduce snail-add numbers))

(apply max (map (fn [[n1 n2]] (magnitude (snail-add n1 n2)))
                (pairs numbers)))
