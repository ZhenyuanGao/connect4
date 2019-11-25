#lang slideshow
(require racket/class
         racket/gui/base)
(require (prefix-in 2dtp: 2htdp/image))
(require "Connect_four_window.rkt")
(require "game-functions.rkt")
(require dyoo-while-loop)

(define rows 6)
(define columns 7)
(define game-state (new state% [current-board (reset-board)] [current-turn 1] [current-status 1]))

;(drop-piece game-state 0 5 columns)
;(for ([i '(0 1 2)])
;  (print (drop-piece game-state 1 5 columns))
;  (print " ")
;  (print (drop-piece game-state 0 5 columns))
;  (print " "))

; game loop
;(define (game-loop)
  
;(add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                           (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))))
;)

; game loop

;make a scanner function to get the user input. In this case, it transform a string to a number.
(define (scanner) (display "Please insert your input")(string->number (read-line (current-input-port) 'any)))

;print the board for the first time
(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
               (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
                           
;while winning conditions are not satisfied, we keep doing the game loop
(define (game-loop)
  (if (check-win game-state 0 columns)
      0
      (let ()
        (while  (= (drop-piece game-state (scanner) 5 columns) -2)(continue)  )
                        

        ;(send game-state get-board))
        ;change turn after each player played and get the newest game state.

        (check-win game-state 0 columns)
        (display (check-win game-state 0 columns))
        (send game-state change-turn)
        ;check the win condition again
  
        ;print out the board again.
        (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
        (game-loop)))) 
 
 
  ;(display "you win lol")
  


;(define (game-loop) (cond [(not (check-win game-state 0 columns))(let () ()     )]))

;show the board object
;(send f show #t)
;
;(drop-piece game-state (scanner) 5 columns)
;(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
;(drop-piece game-state (scanner) 5 columns)
;(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
;(drop-piece game-state (scanner) 5 columns)
;(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
;(drop-piece game-state (scanner) 5 columns)
;(maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))


;(check-win game-state 0 columns)
;call game loop
 (game-loop )
;()
;(while  (= (drop-piece game-state (scanner) 5 columns) -2)(continue)  )
;                         (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                                                    (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))
;while user did not give a valid input, keep asking the user to input again.
;(while  (= (drop-piece game-state (scanner) 5 columns) -2)(continue)  )