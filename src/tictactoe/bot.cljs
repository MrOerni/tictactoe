(ns tictactoe.bot)

(def board-size 3)

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(defn possible-moves [player board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")]
                          [j i])]
    (when remaining-spots
      (map #(assoc-in board % player) remaining-spots))))

(defn mirror [board] (apply mapv vector board))

(defn diagonals [board]
  "Returns the two diagonals in the 3x3 tictactoe board"
  (vector
   (mapv #(get-in board %) [[0 0] [1 1] [2 2]])
   (mapv #(get-in board %) [[0 2] [1 1] [2 0]])))

(defn rows [board] board)
(defn cols [board] (mirror board))

(defn all-lines [board]
  (apply concat ((juxt rows cols diagonals) board)))

(defn win?
  ([board] (or (win? "P" board) (win? "C" board)))
  ([player board]
   (some identity (map #(every? #{player} %) (all-lines board)))))

(defn end-state? [board]
  (or (full? board) (win? board)))

(defn swap-player [player]
    (if (= player "C") "P" "C"))

(defn minimax-tree [player board]
  (if (end-state? board)
    {:state board
     :value (cond
              (win? "C" board) 1
              (win? "P" board) -1
              (full? board) 0)}
    (let [sp (swap-player player)
          children (map #(minimax-tree sp %) (possible-moves player board))]
        {:state board
         :whos-turn player
         :children children
         :value (apply (if (= player "C") max min) (map :value children))})))

(defn move [board]
  (let [root (minimax-tree "C" board)]
    (:state
     (if-let [children (:children root)]
       (first (filter #(= (:value %) (:value root)) children))
       root))))

