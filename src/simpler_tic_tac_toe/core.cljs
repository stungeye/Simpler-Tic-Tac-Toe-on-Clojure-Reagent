(ns simpler-tic-tac-toe.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def app-consts
  {:min-board-size 3
   :max-board-size 9
   :min-win-length 3
   :max-win-length 7})

(defn blank-board [n]
  (vec (repeat n (vec (repeat n :blank)))))

(defonce app-state
  (let [{:keys [min-board-size min-win-length]} app-consts]
    (atom {:board  (blank-board min-board-size)
           :game-status :active
           :win-length  min-win-length})))

(defn update-board! [new-board new-game-status]
  (swap! app-state assoc :board new-board)
  (swap! app-state assoc :game-status new-game-status))

(defn reset-app-state! [board-size win-length]
  (swap! app-state assoc :win-length win-length)
  (update-board! (blank-board board-size) :active))

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

(defn wins? [board win-length player]
  (let [player-positions (board-spaces-of-type board player)]
    (some true?
      (map #(start-of-n-length-run? board % win-length player) player-positions))))

(defn draw? [board]
  (empty? (board-spaces-of-type board :blank)))

(defn determine-game-status [board win-length]
  (cond
    (wins? board win-length :x) :x-wins
    (wins? board win-length :o) :o-wins
    (draw? board)               :draw
    :else                       :active))

(defn cpu-move [board win-length]
  (let [[row column] (rand-nth (board-spaces-of-type board :blank))
        new-board (assoc-in board [row column] :o)
        new-game-status (determine-game-status new-board win-length)]
    (update-board! new-board new-game-status)))

(defn player-move [board row column win-length]
  (let [new-board (assoc-in board [row column] :x)
        new-game-status (determine-game-status new-board win-length)]
    (update-board! new-board new-game-status)
    (if (= new-game-status :active)
      (cpu-move new-board win-length))))

(defn played-space-component [player]
  [:button {:disabled "disabled"} player])

(defn blank-space-component [row column]
  (let [{:keys [board win-length game-status]} @app-state]
    (if (= game-status :active)
      [:button {:on-click #(player-move board row column win-length)} "_"]
      (played-space-component "_"))))

(defn board-component-at [board row column]
 (case (get-in board [row column])
   :blank [blank-space-component row column]
   :x     [played-space-component "X"]
   :o     [played-space-component "O"]))

(defn gameboard-component []
  (let [{:keys [board]} @app-state
        board-size (count board)]
    [:div.board
     (for [row (range board-size)]
       ^{:key row}
       [:p
         (for [column (range board-size)]
           ^{:key column}
           [board-component-at board row column])])]))

(defn select-component [value-atom options]
  [:select
    {:on-change #(reset! value-atom (int (-> % .-target .-value)))}
    (for [option options]
      ^{:key option} [:option option])])

(defn new-game-component []
  (let [{:keys [min-board-size
                min-win-length
                max-board-size
                max-win-length]} app-consts
        selected-size (atom min-board-size)
        selected-win-length (atom min-win-length)]
    [:div.new-game-component
     [:label "Board Size"
       [select-component selected-size (range min-board-size (inc max-board-size))]]
     [:label "Win Length"
       [select-component selected-win-length (range min-win-length (inc max-win-length))]]
     [:button
      {:on-click #(reset-app-state! @selected-size @selected-win-length)}  "new game"]]))

(defn status-component [game-status]
  (let [{:keys [game-status]} @app-state]
    (case game-status
      :x-wins [:p "X Wins!"]
      :o-wins [:p "O Wins!"]
      :draw   [:p "Draw"]
      :active [:p "Game On"])))

(defn tic-tac-app []
  [:div
   [:h1 "Tic Tac Toe"]
   [status-component]
   [gameboard-component]
   [new-game-component]])

(reagent/render-component [tic-tac-app]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
