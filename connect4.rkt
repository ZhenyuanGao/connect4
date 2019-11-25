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
(define (scanner) (display "Please insert your input")(string->number (read-line (current-input-port) 'any)))


 
(define (game-loop) (while (not (check-win game-state 0 columns))
                           (add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board)))

                           (while  (= (drop-piece game-state (scanner) 5 columns) -2)(add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                                                                                                                 (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board))) (continue)  )
                           (send game-state change-turn)

                           (check-win game-state 0 columns)
                           (add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
                                                       (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board)))
 
                           )
  )

(send f show #t)
(game-loop)
;()
