#lang slideshow
(require racket/class
         racket/gui/base)
(require (prefix-in 2dtp: 2htdp/image))
(require "Connect_four_window.rkt")
(require "game-functions.rkt")
(require "connect4ai.rkt")

(define rows 6)
(define columns 7)
(define game-state (new state% [current-board (reset-board rows columns)] [current-turn 1] [current-status 1]))

;make a scanner function to get the user input. In this case, it transform a string to a number.
(define (scanner) (display "Please insert your input")(string->number (read-line (current-input-port) 'any)))

;println the board for the first time
(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) rows)
               (sort (ch  (for/list ([i (in-range 20 130 20)]) i) columns) <) (send game-state get-board))
                           
;while winning conditions are not satisfied, run the game
(define (game-loop)
  (if (= (send game-state get-status) 0) ; exit when game no longer running
      (displayln "See ya next time!")
      (let ()
        (if (equal? (send game-state get-turn) 1) ; check to see who goes
            #| Human Goes |#
            (let ()
              ;(while  (= (drop-piece game-state (scanner) 5 columns) -2) (continue))
              (displayln "Turn: Human")
              (human-play game-state (- rows 1) columns); human making a move

              (println (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) rows)
                                      (sort (ch  (for/list ([i (in-range 20 130 20)]) i) columns) <) (send game-state get-board))) ; print board
              (cond [(= (check-win game-state 0 columns) 1) (let ()(send game-state change-status 0)(displayln "You win!"))] ; check for win condition
                    [(= (check-win game-state 0 columns) 2) (let ()(send game-state change-status 0)(displayln "Computer wins."))]
                    [(= (check-win game-state 0 columns) 3) (let ()(send game-state change-status 0)(displayln "It is a draw."))])
              (send game-state change-turn) ; change turn

              (game-loop))
            
            #| Computer Goes |#
            (let ()
              (displayln "Turn: Computer")
              (drop-piece game-state (get-best-action game-state columns) (- rows 1) columns) ; computer making a move

              (println (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) rows)
                                      (sort (ch  (for/list ([i (in-range 20 130 20)]) i) columns) <) (send game-state get-board))) ; print board
              (cond [(= (check-win game-state 0 columns) 1) (let ()(send game-state change-status 0)(displayln "Human wins!"))] ; check for win condtion
                    [(= (check-win game-state 0 columns) 2) (let ()(send game-state change-status 0)(displayln "Computer wins!"))]
                    [(= (check-win game-state 0 columns) 3) (let ()(send game-state change-status 0)(displayln "It is a draw."))])
              (send  game-state change-turn) ; change turn

              (game-loop))))))

(game-loop)