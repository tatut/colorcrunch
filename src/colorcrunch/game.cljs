(ns colorcrunch.game
  (:require [cljs.core.async :refer [>! put! <! alt! alts! chan timeout close!]]
            
            [colorcrunch.board :as board]
            [colorcrunch.dom :as dom]
            )
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def game-state (atom {:state :menu ;; one of :menu, :play
                       :score {:last nil :high 0}
                       :game {:board nil
                              :board-elts []
                              :score nil
                              :moves-left nil}}))

(def uic (chan)) ;; UI commands channel


(defn game-div []
  (-> js/document (.getElementById "colorcrunch")))


(defn show-menu []
  (let [gd (game-div)
        add! #(.appendChild gd %)
        deg-step (int (/ 120 10)) ;; step size for 12 characters
        ]
    
    ;; I got a little sidetracked here because I wanted to have arced text
    (loop [deg -60 ;; arc from -60 to 60 degrees
           [ch & chs] "ColorCrunch"]
      (when ch
        (add!
          (dom/elt "div"
                   {:class "arced"
                    :style 
                    (let [pos-rad (* (- deg 90) (/ Math/PI 180))]
                      (str "-webkit-transform: rotate(" deg "deg);"
                           " transform: rotate(" deg "deg);"
                           " position: absolute; "
                           " left: " (int (+ 160 (* 100 (Math/cos pos-rad)))) "px;"
                           " top: " (int (+ 200 (* 100 (Math/sin pos-rad)))) "px;"
                           ))
                    }
                   ch))
        (recur (+ deg deg-step)
               chs)))
    ;; Add chronicles text under the arc
    (add! (dom/elt "div"
                   {:class "subtitle"}
                   "CHRONICLES"))
      
    
    (add!
      (doto (dom/elt "a"
                     {:class "menu"}
                     "Play!")
        (.addEventListener "click" #(put! uic [:start-game]))))))


(defn clear-game-div! []
  (set! (.-innerHTML (game-div)) ""))

(defn mobile? []
  (if (re-find #"Android|iPhone|iPad|iPod" js/navigator.userAgent)
      true
      false))

(defn show-board [board gc]
  "Create the DOM elements for the game board. Takes a board and a channel
where UI events should be sent."
  ;;(println "show-board: " board)
  (let [events (if (mobile?)
                 [:touchstart] ;; :touchend]
                 [:click])]
    (loop [acc []
           row []
           x 0 
           y 0]
      (if (= y 9)
        acc
        (let [pc (nth board (+ (* y 9) x))
              handler (fn [e]
                        (.preventDefault e)
                        (put! gc [:select x y]))
              div (doto (dom/elt "div" {})
                    (.setAttribute "class"
                      (str "p" x " piece " (name (:type pc)))))
              row (conj row div)]
          (doseq [e events]
            (dom/listen div e handler))
          (when (= x 8)
            ;; we have a full row, create and append
            (.appendChild (game-div)
              (doto (apply dom/elt "div" {} row)
                (.setAttribute "class" (str "row r" y)))))
          
          (recur (conj acc div)
                 (if (= x 8) [] row)
                 (if (= x 8) 0 (+ x 1))
                 (if (= x 8) (+ y 1) y)))))))

(defn show-level [level]
  "Create DOM elements for the level information. Returns vector containing
elements for the score and removed values."
  (let [gd (game-div)
        score (dom/elt "div" {:class "score"})
        removed (dom/elt "div" {:class "removed"})]
    (.appendChild gd
      (dom/elt "div" {:class "info"}
               (dom/elt "div" {:class "level"}
                        (str (:level level)))
               score
               removed 
               (dom/elt "div" {:class "goal"}
                        (str (:goal level)))))
    [score removed]))
               
(def +animation-duration+ 
  "Duration of animations in milliseconds."
  175)

(defn animation-style [anim duration easing]
  "Create a style attribute that sets given animation"
  (str "-webkit-animation: " duration "ms " anim " " easing ";"
       "animation: " duration "ms " anim " " easing ";" 
       ))

(defn set-animation! 
  ([d animation] (set-animation! d animation +animation-duration+ ""))
  ([d animation duration easing]
    (.setAttribute d "style" "")
    (set! (.-offsetWidth d) (.-offsetWidth d)) ; trigger reflow
    (.setAttribute d "style" 
        (animation-style animation duration easing))))

(defn update-board [board-ui old-board new-board animations]
 
  (loop [[d & divs] board-ui
         [o & olds] old-board
         [n & news] new-board
         idx 0]
    (when d
      (when (not (= o n))
        ;; board changed at this position, update DOM
        (.setAttribute d "class"
          (str "piece " (name (:type n))
               (when (:selected n)
                 " selected")
               (when-let [special (:state n)]
                 (str " " (name special)))
               )))
      (if-let [animation (animations idx)]
        
        (case (first animation)
          
          :selected 
          (set-animation! d "pulsate" 1000 "ease-in-out")
          
          :moved-from
          (set-animation! d (str "from-" (name (second animation))))
          
          :reject-move
          (set-animation! d "reject-move") ;; fixme: use dir for better anim
          
          :crunch 
          (set-animation! d "crunch")
          
          :drop
          (set-animation! d "drop")
          )
        
        ;; No animation here, set style to empty
        (.setAttribute d "style" ""))
      
      (recur divs olds news (inc idx)))))  

(defn animate-board-win [divs]
  (doseq [d divs]
    (set-animation! d "crunch")))
  
(defn animate-board-help [divs move]
  (doseq [p (seq move)]
    (set-animation! (nth divs p) "help" 1000 "ease-in-out"))
  (timeout 1000))

(defn game-loop [level]
  "Initialize a new game for the given level. Returns a channel where the
result of the game (lost, won, points, etc) will be written and then closed."
  
  (let [level (-> level
                (assoc :score 0)
                (assoc :removed 0))
        ;; Create board with level specs
        board (board/create-random-board-without-runs level)
        ;; Channel for the game end result
        game-result-chan (chan)
        ;; Channel for UI input to the game
        gc (chan)]
    (clear-game-div!)
    
    (let [board-ui (show-board board gc)
          [score-ui removed-ui] (show-level level)]      
      (go
        (loop [old-level level
               level level
               old-board board ;; previous board
               board board ;; current board
               animations {}]
          
          ;; Update level and score
          (dom/set-text! score-ui (or (:score level) 0))
          (dom/set-text! removed-ui (or (:removed level) 0))
          
          (let [wait (timeout +animation-duration+)]
            ;; Update game board to DOM and setup animations
            ;; Note: calling update-board inside requestAnimationFrame callback
            ;; doesn't seem to have any effect
            (update-board board-ui old-board board animations)
            ;; If we set up any animations, wait for them to finish
            (when (not (empty? animations))
              (<! wait)))
            
          ;; Check board for changes
          (let [[new-level new-board anims] (board/update level board)]
            (if (not (= new-board board))
              ;; Board has changed, recur now to run animations
              (recur
                level
                new-level
                board
                new-board
                anims)
              
              ;; check win condition
              (if (>= (:removed level) (:goal level))
                ;; You won this level! Yay, end this loop here and write result
                (do (animate-board-win board-ui)
                  (<! (timeout +animation-duration+))
                  (clear-game-div!)
                  (>! game-result-chan 
                    {:result :win :score (:score level)}))
                
                ;; Check lose condition (no more possible moves)
                (let [first-possible-run (board/find-first-possible-run board)]
                  ;; (println "first-possible-run: " first-possible-run)
                  (if (nil? first-possible-run)
                    ;; You lose
                    (do 
                      (clear-game-div!)
                      (>! game-result-chan
                          {:result :lose}))
                    
                    ;; Board is stable, the game is not won or lost
                    ;; we are ready to take player input
                    ;; if player doesn't issue any command in 4s, show hint                    
                    (let [[val port] (alts! [(timeout 4000) gc])]
                      (if (not (= port gc))
                        (do 
                          (<! (animate-board-help board-ui first-possible-run))
                          (recur level level board board {}))
                        
                        (let [[input & args] val]
                          ;;(println "got input " input " and args " args)
                          (case input                
                            :select
                            (let [[new-board anims] (board/select-piece board args)]
                              ;;(println "new-board: " board)
                              ;;(println "animations: " anims)
                              (recur 
                                level level
                                board new-board
                                anims))
                            ))))))))))))
      game-result-chan
      ))
            
(def +levels+
  [{:level 1
    :piece-types 5
    :goal 40}
   
   {:level 2
    :piece-types 6
    :goal 60}
   
   {:level 3
    :piece-types 7
    :goal 60}
   
   {:level 4
    :piece-types 8
    :goal 60}
   
   {:level 5
    :piece-types 8
    :goal 100}
   
   {:level 6
    :piece-types 8
    :goal 150}
   
   {:level 7
    :piece-types 8
    :goal 250}
   ])

(defn make-level [lvl]
  (if (>= lvl (count +levels+))
    (let [last-level (last +levels+)]
      ;; Wohoo, infinite levels
      (merge last-level
             {:level lvl
              :goal (+ (:goal last-level) (* 10 lvl))
              }))
    (nth +levels+ lvl)))

(defn play []
  (go 
    (loop [current-level 0
           total-score 0]
      (let [level (make-level current-level)
            result (<! (game-loop 
                         level))]
        (case (:result result)
          :win
          (recur (+ current-level 1)
                 (+ total-score (:score result)))
          
          :lose
          (do (.alert js/window (str "Game over on level " level 
                                     " with total score " total-score))
            (put! uic [:init])))))))

;; The main UI loop
(go 
 (loop [[cmd & args] (<! uic)]
   (println "Got UI command " cmd " with args " args)
   (case cmd
     
     :init
     (do 
       (clear-game-div!)
       (show-menu))
     
     :start-game
     (play)
     )
   (recur (<! uic))))

;; When DOM is ready, send UI command to show the menu
(set! (.-onload js/window) #(put! uic [:init]))


