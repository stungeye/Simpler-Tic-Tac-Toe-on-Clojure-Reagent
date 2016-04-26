(ns simpler-tic-tac-toe.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def board-size 3)

(defn blank-board [n]
  (vec (repeat n (vec (repeat n :blank)))))

(defonce app-state
  (atom {:board  (blank-board board-size)
         :game-status :active}))

(defn update-app-state! [new-board new-game-status]
  (swap! app-state assoc :board new-board)
  (swap! app-state assoc :game-status new-game-status))

(defn reset-app-state! []
  (update-app-state! (blank-board board-size) :active))

(defn board-positions [board-size]
  (for [x (range board-size) y (range board-size)] [x y]))

(defn board-spaces-of-type [board type]
  (let [board-size (count board)
        positions (board-positions board-size)]
     (filter #(= type (get-in board %)) positions)))

(defn start-of-n-length-run? [board position n player]
  (let [[row column] position]
    (some true? ; Is position the start of at least one run of length n?
      (for [[delta-row delta-column] [[0 1] [1 0] [1 1] [1 -1]]]
        (every? true? ; Check down, right, and both downward diagonals for runs.
          (for [i (range n)]
            (= (get-in board [(+ (* delta-row i) row) (+ (* delta-column i) column)])
               player)))))))

(defn wins? [board player]
  (let [player-positions (board-spaces-of-type board player)]
    (some true?
      (map #(start-of-n-length-run? board % board-size player) player-positions))))

(defn draw? [board]
  (empty? (board-spaces-of-type board :blank)))

(defn determine-game-status [board]
  (cond
    (wins? board :x) :x-wins
    (wins? board :o) :o-wins
    (draw? board)    :draw
    :else            :active))

(defn cpu-move [board]
  (let [[row column] (rand-nth (board-spaces-of-type board :blank))
        new-board (assoc-in board [row column] :o)
        new-game-status (determine-game-status new-board)]
    (update-app-state! new-board new-game-status)))

(defn player-move [board row column]
  (let [new-board (assoc-in board [row column] :x)
        new-game-status (determine-game-status new-board)]
    (update-app-state! new-board new-game-status)
    (if (= new-game-status :active)
      (cpu-move new-board))))

(defn disabled-space-component [player]
  [:button {:disabled "disabled"} player])

(defn blank-space-component [row column]
  (let [{:keys [board game-status]} @app-state]
    (if (= game-status :active)
      [:button {:on-click #(player-move board row column)} "_"]
      (disabled-space-component "_"))))

(defn board-component-at [board row column]
 (case (get-in board [row column])
   :blank [blank-space-component row column]
   :x     [disabled-space-component "X"]
   :o     [disabled-space-component "O"]))

(defn gameboard-component []
  (let [{:keys [board game-status]} @app-state
        board-size (count board)]
    [:div.board
     (for [row (range board-size)]
       ^{:key row}
       [:p
         (for [column (range board-size)]
           ^{:key column}
           [board-component-at board row column])])]))

(defn status-component [game-status]
  (let [{:keys [game-status]} @app-state]
    (case game-status
      :x-wins [:p "X Wins!"]
      :o-wins [:p "O Wins!"]
      :draw   [:p "Draw"]
      :active [:p "Game On"])))

(defn new-game-component []
     [:button {:on-click #(reset-app-state!)}  "new game"])

(defn tic-tac-app []
  [:div
   [:h1 "Tic Tac Toe"]
   [status-component]
   [gameboard-component]
   [new-game-component]])

(reagent/render-component [tic-tac-app]
                          (. js/document (getElementById "app")))

(defn on-js-reload []) ; Nothing doing.
