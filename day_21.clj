(ns day-21)

(def state {:rnd 1
            :rolls 0
            :p1 {:pos 8
                 :score 0}
            :p2 {:pos 3
                 :score 0}})

(def max-score 21)

(defn other-player [player]
  (if (= player :p1) :p2 :p1))

(defn roll-dice [state]
  (let [value (:rnd state)
        state (-> state
                  (update :rnd inc)
                  (update :rolls inc))]
    [state value]))

(defn roll-multiple-dice [state n]
  (nth (iterate (fn [[state dice]]
                  (let [[state* dice*] (roll-dice state)]
                    [state* (+ dice dice*)]))
            [state 0])
       n))

(defn step-position [position steps]
  (inc (rem (+ position steps -1) 10)))

(defn move-player [state player steps]
  (let [pos (get-in state [player :pos])
        new-pos (step-position pos steps)]
    (-> state
        (assoc-in [player :pos] new-pos)
        (update-in [player :score] + new-pos))))

(defn take-turn [state player]
  (let [[state steps] (roll-multiple-dice state 3)
        state (move-player state player steps)]
    state))

(defn winner [state]
  (cond (>= (get-in state [:p1 :score]) max-score) :p1
        (>= (get-in state [:p2 :score]) max-score) :p2
        :else nil))

(defn play-game [state player]
  (if (winner state)
    (* (:rolls state)
       (get-in state [(other-player (winner state)) :score]))
    (play-game (take-turn state player)
               (other-player player))))

(def dice-freqs
  (frequencies (for [a (range 1 4)
                     b (range 1 4)
                     c (range 1 4)]
                 (+ a b c))))

(defn map-keys [m f]
  (zipmap (keys m) (map f (vals m))))

(def multiverse
  (memoize
   (fn [state player]
     (let [winner (winner state)]
       (if winner {winner 1}
           (reduce (partial merge-with +)
                   (map #(map-keys (multiverse (move-player state player %)
                                               (other-player player))
                                   (partial * (dice-freqs %)))
                        (keys dice-freqs))))))))

;; (play-game state :p1)
(apply max (vals (multiverse state :p1)))
