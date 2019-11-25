#lang racket
;(require "connect4.rkt")
(require racket/vector)
(require "game-functions.rkt")
;(require "Connect_four_window.rkt")
(provide (all-defined-out))

(define greeting "Hello human.")
(define (change-greeting)
  (set! greeting "Goodbye human."))

(define (greet-ai)
  (print "Hello human."))

; 0 0 0 2 0 0 0 2 1 2 1 2 1 2 2 1 2 1 2 1 2 1 2 1 1 2 2 1 1 1 1 2 1 2 1 2 2 1 1 1 2 1

(define game-state (new state% [current-board (vector 0 0 0 0 0 0 0
                                                      0 0 0 0 0 0 0
                                                      0 0 0 0 0 0 0
                                                      0 0 0 0 1 0 0
                                                      0 0 2 1 1 0 0
                                                      0 0 1 2 1 0 0)] [current-turn 2] [current-status 1]))

(define (calculate-dir-value board turn prev-position columns get-next-pos)
  (define position (get-next-pos prev-position columns))
  (cond
    ; return with no added value if continuing means going off of the board
    [(or (and (equal? (modulo prev-position columns) 0) ; check if previous piece is at left border
              (equal? (modulo position columns) (- columns 1))) ; check if the next piece in the direction would pass border
         (and (equal? (modulo prev-position columns) (- columns 1)) ; check if piece is at right border
              (equal? (modulo position columns) 0)) ; check if the next piece in the direction would pass border
         (> position (- (vector-length board) 1))) 0] ; pass bottom border
    ; if piece is the correct color, continue add value and continue in same direction
    [(equal? (vector-ref board position) turn) (+ (check-dir board turn position columns get-next-pos) 1)]
    ; if piece is a blank space after a correct color piece, return with added value
    [(and (equal? (vector-ref board prev-position) turn) (equal? (vector-ref board position) 0)) 1]
    ; no added value
    [else 0]))

(define (total-dir-value board turn position columns)
  (apply + (cons (calculate-dir-value board turn position columns (lambda (p n) (+ p (- n 1)))) ; down and left
                 (cons (calculate-dir-value board turn position columns (lambda (p n) (+ p n))) ; down
                       (cons (calculate-dir-value board turn position columns (lambda (p n) (+ p (+ n 1)))) ; down and right
                             (cons (calculate-dir-value board turn position columns (lambda (p n) (+ p 1))) '())))))) ; right

; helper function for calculate-utility
(define (calculate-utility-helper board turn position columns)
  (cond
    [(> position (- (vector-length board) 1)) 0] ; return 0 when the end has been reached
    [(or (equal? (vector-ref board position) turn) (equal? (vector-ref board position) 0)) (+ (total-dir-value board turn position columns) (calculate-utility-helper board turn (+ position 1) columns))] ; if selected position matches player's piece or is empty, check pieces around it for value adding features                                                  
    [else (calculate-utility-helper board turn (+ position 1) columns)])) ; selected position has opponent's piece, so go to next position

; returns the value of a given state as a number
(define (calculate-utility state columns)
  (calculate-utility-helper (send state get-board) (send state get-turn) 0 columns))

(define (get-result-state state action columns)
  (define result-state (new state%
                   [current-board (vector-copy (send state get-board))]
                   [current-turn (send state get-turn)]
                   [current-status (send state get-status)]))
  (drop-piece result-state action 5 columns)
  (send result-state change-turn)
  result-state)

; returns the max value among an integer and a list of integers
(define (max current-max value-list)
  (if (eq? value-list null)
      current-max ; return when list is empty
      (if (> current-max (car value-list))
          (max current-max (cdr value-list))
          (max (car value-list) (cdr value-list)))))

; returns all possbile maximum utilitly values for this state
(define (get-max-values state actions columns)
  (if (eq? actions null)
      '()
      (cons (get-max (get-result-state state (car actions) columns) columns) (get-max-values state (cdr actions) columns))))

; returns the max utility value for this state
(define (get-max state columns)
  (define default-value -100)
  (define win-value (check-win state 0 columns))
  (if (not(equal? win-value 0)) ; check there is a winner or tie
      (cond
        [(equal? win-value 1) -1] ; -1 value computer lost
        [(equal? win-value 2) 1] ; 1 value computer won
        [else 0]) ; 0 value tie
      (max default-value (get-min-values state (get-actions state columns) columns))))

; returns the min value among an integer and a list of integers
(define (min current-min value-list)
  (if (eq? value-list null)
      current-min ; return when list is empty
      (if (< current-min (car value-list))
          (min current-min (cdr value-list))
          (min (car value-list) (cdr value-list)))))

; returns all possible minimum utility values for this state
(define (get-min-values state actions columns)
  (if (eq? actions null)
      '()
      (cons (get-min (get-result-state state (car actions) columns) columns) (get-min-values state (cdr actions) columns))))

; returns the min utility value for this state
(define (get-min state columns)
  (define default-value 100)
  (define win-value (check-win state 0 columns))
  (if (not(equal? win-value 0)) ; check there is a winner or tie
      (cond
        [(equal? win-value 1) -1] ; -1 value computer lost
        [(equal? win-value 2) 1] ; 1 value computer won
        [else 0]) ; 0 value tie
      (min default-value (get-max-values state (get-actions state columns) columns))))

(define (max-value-position values actions)
  (define (max-value-pos-helper max-value values max-action actions)
    ;(print "max val")
    ;(print max-value)
    ;(print "max action")
    ;(print max-action)
    (if (eq? values null)
        max-action ; return when list is empty
        (if (> max-value (car values))
            (max-value-pos-helper max-value (cdr values) max-action (cdr actions))
            (max-value-pos-helper (car values) (cdr values) (car actions) (cdr actions)))))

  (max-value-pos-helper -100 values 0 actions))


; returns the best action or actions if tied
(define (get-best-action state columns)
  (define actions (get-actions state columns))
  ;(print actions)
  (print (get-min-values state actions columns))
  (max-value-position (get-min-values state actions columns) actions))


(define (get-input state column rows columns)
  (when (< (drop-piece state column rows columns) 0)
      (let ()
        (print "try again")
        (get-input state (string->number (read-line)) rows columns))))

(define (gameloop state)
  ;(print (maker-helper (ch (for/list ([i (in-range 20 150 20)]) i) 6)
    ;                                                   (sort (ch (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board)))
  (get-input state (string->number (read-line)) 5 7)
  (when (equal? (check-win state 0 7) 1) (print "Human wins!"))
  (send state change-turn)

  ;(print (maker-helper (ch (for/list ([i (in-range 20 150 20)]) i) 6)
   ;                                                    (sort (ch (for/list ([i (in-range 20 130 20)]) i) 7) <) (send game-state get-board)))
  (print(drop-piece state (get-best-action state 7) 5 7))
  (when (equal? (check-win state 0 7) 2) (print "Computer wins!"))
  (send state change-turn)

  (gameloop state))

  