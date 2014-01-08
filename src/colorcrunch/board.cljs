;; Handle all CC logic related to the game board
(ns colorcrunch.board
  (:require [clojure.set :as sets]))

(def +piece-types+ 
  "The possible piece types, excluding special types"
  [:red :green :blue :yellow :brown :pink :ash :orange])

(defn random-piece 
  ([] {:type (rand-nth +piece-types+)})
  ([types]
    {:type (nth +piece-types+ (rand-int types))}))

(defn create-random-board [{piece-types :piece-types}]
  (loop [board []
         i 0]
    (if (= i 81)
      board
      (recur (conj board (random-piece piece-types)) (inc i)))))

(declare find-runs find-all-runs)

(defn create-random-board-without-runs [level]
  "Create a random board that doesn't initially have any runs. This is better
for a new level so that player doesn't get points by simply starting the level."
  (loop [b (create-random-board level)]
    (if (empty? (find-all-runs b))
      b
      (recur (create-random-board level)))))


(def piece-char {:red "R"
                  :green "G"
                  :blue "B"
                  :yellow "Y"
                  :brown "O"
                  :pink "P"
                  :empty "_"})

(defn- print-board [board]
  "Return a string representation of a board, for debugging"
  (reduce str 
          (map (fn [row]
                 (str (reduce str (map #(piece-char (:type %)) row))
                      "\n"))
               (partition 9 board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing and navigating the board

(defn- to-pos [x y]
  (+ x (* 9 y)))

  
(defn piece-at [board pos]
  "Returns the type of piece on the board at the given location."
  (nth board pos))

(defn swap-pieces [board pos1 pos2]
  "Return board with pieces at the given two positions swapped"
  (let [p1 (nth board pos1)
        p2 (nth board pos2)]
    (-> board
      (assoc pos1 p2)
      (assoc pos2 p1))))

(defn move-up [pos]
  "Return position that is one place above the given coordinates."
  (let [y (int (/ pos 9))]
    (when (> y 0)
      (- pos 9))))

(defn move-down [pos]
  "Return position that is one place below the given coordinates."
  (let [y (int (/ pos 9))]
    (when (< y 8)
      (+ pos 9))))

(defn move-left [pos]
  "Return position that is one place left of the given coordinates."
  (let [x (mod pos 9)]
    (when (> x 0)
      (- pos 1))))

(defn move-right [pos]
  "Return position that is one place right of the given coordinates."
  (let [x (mod pos 9)]
    (when (< x 8)
      (+ pos 1))))

(defn valid-position [pos]
  "Check if given position is within the board (doesnt over or underflow)"  
  (and (not (nil? pos))
       (<= 0 pos 80)))

(defn adjacent-in-direction 
  "Return the direction pos2 is adjacent to pos1. Returns the direction
as keyword, or nil if the two positions are not adjacent"
  [pos1 pos2]
  (and (valid-position pos1)
       (valid-position pos2)
       (or
         (and (= pos2 (move-left pos1)) :left)
         (and (= pos2 (move-right pos1)) :right)
         (and (= pos2 (move-up pos1)) :up)
         (and (= pos2 (move-down pos1)) :down))))

(defn opposite-direction [dir]
  (case dir
    :left :right
    :right :left
    :up :down
    :down :up))
    
(defn currently-selected-position [board]
  (loop [[p & pcs] board
         i 0]
    (if (nil? p)
      nil
      (if (:selected p)
        i
        (recur pcs (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game logic for updating the board according to the game rules
;;
;; The individual rules are functions that take a vector containing:
;; - current level info as map (score, goal, pieces to spawn, etc)
;; - the board (vector of the pieces on the board)
;; - map of animations (keyed by index to the board position vector)
;;
;; Each animation is a vector of animation type and possible arguments.
;;
;; When the board is updated, the values are threaded through all the
;; rule functions.



(defn remove-crunched [[level board anims]]
  "Remove all crunched pieces from the board by replacing them with pieces
whose type is :empty. If there are no crunched pieces, returns the board as is."
  (loop [board board
         idx 0]
    (if (= idx 81)
      [level board anims]
      (if (= :crunch (:state (nth board idx)))
        (recur (assoc board idx {:type :empty})
               (+ idx 1))
        (recur board (+ idx 1))))))

(defn- score-runs [level runs]
  "Update level score with new runs"
  (loop [score 0
         removed 0
         [r & runs] (seq runs)]
    (if (nil? r)
      (-> level
        (assoc :score (+ (:score level) score))
        (assoc :removed (+ (:removed level) removed)))
      (recur
        (+ score
           (case (count r)
             ;; totally arbitrary scoring
             3 60   
             4 360  
             5 1200
             6 9000
             7 10000
             8 11000
             9 12000
             10 15000
             ))
        (+ removed (count r))
        runs))))
          
(defn crunch-match-pieces [[level board anims]]
  "Crunch all pieces that are part of a matching run. Returns the new board
and a map of crunch animations. If there are no matches in the board, the
returned board will be identical to parameter."
  (let [runs (find-all-runs board)]
    ;;(println "CRUNCH MATCH PIECES " runs)
    (if (empty? runs)
      [level board anims]
      
      (let [positions (apply sets/union (seq runs))
            ;; Set all positions in a run to empty
            new-board 
            (reduce (fn [board pos]
                        (assoc board
                               pos (assoc (nth board pos)
                                          :state :crunch)))
                    board positions)
            
            ;; Make animation to crunch all positions
            animations (let [a [:crunch]]
                         (reduce (fn [anims pos]
                                   (assoc anims pos a))
                                 anims positions))]
        ;; Return new board with animations
        [(score-runs level runs) new-board animations]))))

;; Code to reward long runs (4 or up) with special pieces:
;; bomb, lightning or colorbomb.
;; Special pieces not currently implemented. 
;;
;;(loop [board new-board
;;       [r & runs] (seq runs)]
;;  (if (nil? r)
;;    [level board animations]
;;    (let [p (rand-nth (seq r))]
;;      (recur (case (count r)
;;               3 board
;;               4 (assoc board
;;                        p
;;                        (-> (nth board p)
;;                          (assoc :state :bomb)))
;;               5 (assoc board
;;                        p 
;;                        (-> (nth board p)
;;                          (assoc :state :lightning)))
;;               (6 7 8 9 10)
;;               (assoc board
;;                      p
;;                      (-> (nth board p)
;;                        (assoc :type :colorbomb)
;,                        (assoc :state nil))))
;;             runs))))                         


(defn drop-pieces [[level board anims]]
  "Rule to drop pieces downward if the position below is empty."
  (loop [board board
         anims anims
         pos 80]
    (if (= pos 8) ;; stop when we reach the top row
      [level board anims]
      (let [above-pos (move-up pos)
            above (piece-at board above-pos)
            here (piece-at board pos)]
        (if (and (not (= :empty (:type above)))
                 (= :empty (:type here)))
          (recur
            (swap-pieces board pos above-pos)
            (assoc anims pos [:drop])
            (- pos 1))
          (recur board anims (- pos 1)))))))

(defn spawn-pieces-to-top [[level board anims]]
  "Rule to spawn new pieces from above the board if the top row has empty
pieces."
  (loop [board board
         anims anims
         idx 0]
    (if (= idx 9)
      [level board anims]
      (if (= :empty (:type (nth board idx)))
        (do ;;(println "empty piece at " idx ": " (nth board idx))
          (recur (assoc board
                        idx (random-piece))
                 (assoc anims 
                        idx [:drop])
                 (+ idx 1)))
        (recur board anims (+ idx 1))))))

(defn update [level board]
  "Update board state according to level and game rules. If board has any 
changes, return a new board and the animations. If the board is stable and has
no changes, return the board unchanged and an empty map of animations."
  ;;(println "UPDATE")
  ;;(println (print-board board))
  (let [initial board
        updated
        (-> [level board {}]
          remove-crunched
          spawn-pieces-to-top
          drop-pieces)]
    ;; Only crunch match pieces, if the board is not in flux
    ;; (eg. pieces are dropping) 
    (if (= initial (second updated))
      (crunch-match-pieces updated)
      updated)))
  
(defn select-piece [board [x y]]
  "User action: select piece, if piece is already selected, try two swap the
previously selected piece with the new selection (if they are adjacent).
Returns a new board and map of animations to apply to positions"
  ;;(println "select-piece: " board pos)
  (let [pos (to-pos x y)
        current-pos (currently-selected-position board)]
    ;;(println "current sel: " current-pos)
    (if (nil? current-pos)
      ;; No current selection, make the new pos the selected
      [(-> board
         (assoc pos
                (assoc (piece-at board pos) :selected true)))
       {pos [:selected]}]
      
      ;; Current selection, swap positions  
      (let [pos1 current-pos
            pos2 pos
            v1 (piece-at board pos1)
            v2 (piece-at board pos2)
            dir (adjacent-in-direction pos1 pos2)]
        ;;(println pos1 " is adjacent to " pos2 " in direction " dir)
        ;; Accept this swap, if the positions are adjacent
        ;; and the swap results in a match in either position
        (if dir
          (let [new-board (-> board
                            (assoc pos1
                                   (dissoc v2 :selected))
                            (assoc pos2
                                   (dissoc v1 :selected)))]
            ;; Check that the new board has a run in one or more of
            ;; the swapped positions, otherwise don't accept this selection
            (if (or (find-runs new-board pos1)
                    (find-runs new-board pos2))
             ;; We have a run, return the new board and animate the swap
              [new-board
               {pos1 [:moved-from dir]
                pos2 [:moved-from (opposite-direction dir)]}]
             
             ;; No run, don't accept this selection, return old board
             ;; and animate a "clash"
             [(-> board
                (assoc pos1
                       (dissoc v1 :selected)))
              ;; fixme: animate
              {pos1 [:reject-move (opposite-direction dir)]
               pos2 [:reject-move dir]}]))
          
          ;; Positions are not adjacent in some direction,
          ;; change selection to be the new position 
          [(-> board
             (assoc pos1 (dissoc v1 :selected))
             (assoc pos2 (assoc v2 :selected true)))
           {pos2 [:selected]}])))))

         
(defn sort-positions [v]
  (vec 
    (sort v)))


  ;; set version
(defn find-run [board initial-pos move-back move-forward]
  "Returns a run of same pieces starting at initial position
and progressing to the before/after positions. Returns a hash set of position
indexes of all the pieces in the run. If the run has length of less than 3 
(two or less same pieces in a row), returns nil."
  
  (let [piece (:type (piece-at board initial-pos))] 
    (loop [pieces #{initial-pos}
           before-pos (move-back initial-pos)
           after-pos (move-forward initial-pos)]
      (let [before-piece (and (valid-position before-pos) 
                              (:type (piece-at board before-pos)))
            after-piece (and (valid-position after-pos) 
                             (:type (piece-at board after-pos)))]
        (if (and (not (= piece before-piece))
                 (not (= piece after-piece)))
          ;; neither piece at before/after is same, run is over
          (if (< (count pieces) 3)
            nil
            pieces)
          ;; found at least one same, recur further back/forward
          (let [pieces (if (= before-piece piece) 
                         (conj pieces before-pos)
                         pieces)
                pieces (if (= after-piece piece)
                         (conj pieces after-pos)
                         pieces)]
            
            (recur 
              pieces
              (if (= before-piece piece) (move-back before-pos) nil)
              (if (= after-piece piece) (move-forward after-pos) nil))))))))


  ;; set version
(defn find-runs [board position]
  "Determine the runs in the given position. Returns nil of no run is at position."
  (let [vertical-run (find-run board position move-up move-down)
        horizontal-run (find-run board position move-left move-right)]
    
    (when (or vertical-run horizontal-run)
      (concat 
        (when vertical-run [vertical-run])
        (when horizontal-run [horizontal-run])))))

(defn- combine-runs [runs]
  "Combine runs that contain the same position"
  (if (<= (count runs) 1)
    ;; at most one run, no combinations possible
    runs
    ;; try to combine the first run with all the others
    (let [[first-run & other-runs] (seq runs)]
      ;;(println "Combine runs, first: " first-run ", other runs: " other-runs)
      (if-let [matching (some (fn [r]
                                (let [intersect 
                                      (sets/intersection first-run r)]
                                  (if (empty? intersect)
                                    nil
                                    r)))
                              other-runs)]
        ;; We have a matching run, combine and check again
        (combine-runs 
          (-> runs 
            (conj (sets/union first-run matching))
            (disj first-run)
            (disj matching)))
        
        ;; No matching run, check the rest without the first one
        (conj (combine-runs 
                (disj runs first-run))
              first-run)))))

(defn- check-achievable-run [board x y fwd left right]
  (let [initial-pos (to-pos x y)
        start (:type (piece-at board initial-pos))
        pos (fn [& moves]
              (loop [p initial-pos
                     [m & moves] moves]
                (if (or (nil? m) (nil? p))
                  p
                  (recur (m p) moves))))
        same? (fn [& moves]
                (let [p (apply pos moves)]
                  (when (valid-position p)
                      (= start (:type (piece-at board p))))))]
    
    (cond
      
      ;; Case: 
      ;; X  
      ;;  X
      ;;  X
      (and (same? fwd)
           (same? fwd fwd left))
      (do (println "match1")
      #{initial-pos (pos fwd) (pos fwd fwd left)})
      
      ;;  X
      ;; X
      ;; X
      (and (same? fwd)
           (same? fwd fwd right))
      (do (println "match2")
        #{initial-pos (pos fwd) (pos fwd fwd right)})
        
      ;; XX X
      (and (same? fwd)
           (same? fwd fwd fwd))
      (do (println "match3")

      #{initial-pos (pos fwd) (pos fwd fwd fwd)})
      
      ;;  X
      ;; X X
      (and (same? fwd left)
           (same? fwd fwd))
            (do (println "match4")
              #{initial-pos (pos fwd left) (pos fwd fwd)})
      
      ;; X X
      ;;  X
      (and (same? fwd right)
           (same? fwd fwd))
      (do (println "match5")
        #{initial-pos (pos fwd right) (pos fwd fwd)})
      
      :default nil)))
    
(defn find-first-possible-run [board]
  "Finds and returns the first possible run that can be achieved with one
move. If there are no achievable runs, returns nil (game over)."
  (loop [x 0
         y 0]
    (if (= y 9)
      nil
      (let [move (or (check-achievable-run board x y
                                           move-left move-up move-down)
                     (check-achievable-run board x y
                                           move-right move-up move-down)
                     (check-achievable-run board x y
                                           move-up move-left move-right)
                     (check-achievable-run board x y
                                           move-down move-left move-right))]
        (if move
          move
          (recur 
            (if (= x 8) 0 (inc x))
            (if (= x 8) (inc y) y)))))))
      
(defn find-all-runs [board]
  "Finds all runs on the board. Combines runs that have the same index in them." 
  (loop [runs #{}
         x 0
         y 0]
    ;;(println " find all runs at X: " x ", Y: " y ", runs: " runs)
    (if (= 9 y)
      (combine-runs runs)
      (recur (reduce conj runs
                     (find-runs board (to-pos x y)))
             (if (= x 8) 0 (inc x))
             (if (= x 8) (inc y) y)))))
      
  
