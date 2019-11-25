#lang slideshow
(require racket/class
         racket/gui/base)
(require (prefix-in 2dtp: 2htdp/image))
(require "Connect_four_window.rkt")
(require "game-functions.rkt")
(require "connect4ai.rkt")
;(require dyoo-while-loop)

(define rows 6)
(define columns 7)
(define game-state (new state% [current-board (reset-board 6 7)] [current-turn 1] [current-status 1]))

;(drop-piece game-state 0 5 columns)
;(for ([i '(0 1 2)])
;  (println (drop-piece game-state 1 5 columns))
;  (println " ")
;  (println (drop-piece game-state 0 5 columns))
;  (println " "))

; game loop
;(define (game-loop)
  
;(add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                           (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))))
;)

; game loop

;make a scanner function to get the user input. In this case, it transform a string to a number.
(define (scanner) (display "Please insert your input")(string->number (read-line (current-input-port) 'any)))

;println the board for the first time
;(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;               (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
                           
;while winning conditions are not satisfied, we keep doing the game loop
;check the status of the game
(define (game-loop)
  (if (= (send game-state get-status) 0)
      0
      (let ()
        (println (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                              (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))) ; print board
        ;(while  (= (drop-piece game-state (scanner) 5 columns) -2) (continue))
        (display "Turn: ")
        (displayln (if (equal? (send game-state get-turn) 1) "Human" "Computer"))
        (human-play game-state (- rows 1) columns); human making a move

        ;(send game-state get-board))
        ;change turn after each player played and get the newest game state.
        ;checkwin 0 keep running game status is 0 stop running. When checkwin has three different input 1 means player1 wins,2 means player 2 wins,3 means it is a tie. for winning.

        (cond [(= (check-win game-state 0 columns) 1) (let ()(send game-state change-status 0)(display "Human wins!"))]
              [(= (check-win game-state 0 columns) 2) (let ()(send game-state change-status 0)(display "Computer wins!"))]
              [(= (check-win game-state 0 columns) 3) (let ()(send game-state change-status 0)(display "It is a draw."))])
              ;[(= (check-win game-state 0 columns) 0) (display (check-win game-state 0 columns))])
        (send game-state change-turn) ; change turn

        (println (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                                (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))) ; print board
        (display "Turn: ")
        (displayln (if (equal? (send game-state get-turn) 1) "Human" "Computer"))
        (drop-piece game-state (get-best-action game-state columns) (- rows 1) columns) ; computer making a move
        (cond [(= (check-win game-state 0 columns) 1) (let ()(send game-state change-status 0)(display "Human wins!"))]
              [(= (check-win game-state 0 columns) 2) (let ()(send game-state change-status 0)(display "Computer wins!"))]
              [(= (check-win game-state 0 columns) 3) (let ()(send game-state change-status 0)(display "It is a draw."))])
              ;[(= (check-win game-state 0 columns) 0) (display (check-win game-state 0 columns))])
        (send  game-state change-turn) ; change turn
        
        (game-loop)))) 
 
 
  ;(display "you win lol")
(game-loop)
;()
;(while  (= (drop-piece game-state (scanner) 5 columns) -2)(continue)  )
;                         (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                    (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
;while user did not give a valid input, keep asking the user to input again.
;(while  (= (drop-piece game-state (scanner) 5 columns) -2)(continue)  )