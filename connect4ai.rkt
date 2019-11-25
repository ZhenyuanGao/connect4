#lang racket
;(require "connect4.rkt")
(require racket/vector)
(require "game-functions.rkt")
;(require "Connect_four_window.rkt")
(provide (all-defined-out))

(define max-depth 5)
(define game-state (new state% [current-board (make-vector 42 0)] [current-turn 1] [current-status 1]))

#| UTILITY FUNCTION STUFF |#

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

; returns the total value of the piece in the selected position
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


#| SEARCH STUFF |#

(define (cutoff-test depth)
  (if (equal? depth max-depth) #t #f))

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

; returns the max utility value for this state
(define (get-max state depth columns)
  (define default-value -100000)
  (define win-value (check-win state 0 columns))
  (cond
    [(not(equal? win-value 0)) (cond ; check there is a winner or tie
                                 [(equal? win-value 1) (+ (- (calculate-utility state columns) 1000))] ; return utility value - 1000 for losing state
                                 [(equal? win-value 2) (+ 1000 (calculate-utility state columns))] ; return utility value + 1000 for winning state
                                 [else (calculate-utility state columns)])] ; return utility value for tie state
    [(cutoff-test depth) (calculate-utility state columns)] ; return the value of the state at the max depth search
    [(max default-value (get-min-values state (get-actions state columns) (+ depth 1) columns))]))

; returns all possbile maximum utilitly values for this state
(define (get-max-values state actions depth columns)
  (if (eq? actions null)
      '()
      (cons (get-max (get-result-state state (car actions) columns) depth columns) (get-max-values state (cdr actions) depth columns))))

; returns the min value among an integer and a list of integers
(define (min current-min value-list)
  (if (eq? value-list null)
      current-min ; return when list is empty
      (if (< current-min (car value-list))
          (min current-min (cdr value-list))
          (min (car value-list) (cdr value-list)))))

; returns the min utility value for this state
(define (get-min state depth columns)
  (define default-value 100000)
  (define win-value (check-win state 0 columns))
  (cond
    [(not(equal? win-value 0)) (cond ; check there is a winner or tie
                                 [(equal? win-value 1) (+ (- (calculate-utility state columns) 1000))] ; return utility value - 1000 for losing state
                                 [(equal? win-value 2) (+ 1000 (calculate-utility state columns))] ; return utility value + 1000 for winning state
                                 [else (calculate-utility state columns)])] ; return utility value for tie state
    [(cutoff-test depth) (calculate-utility state columns)] ; return the value of the state at the max depth search
    [(min default-value (get-max-values state (get-actions state columns) (+ depth 1) columns))]))

; returns all possible minimum utility values for this state
(define (get-min-values state actions depth columns)
  (if (eq? actions null)
      '()
      (cons (get-min (get-result-state state (car actions) columns)depth columns) (get-min-values state (cdr actions) depth columns))))

; returns that action that yields the state with the highest value
(define (max-value-position values actions)
  (define (max-value-pos-helper max-value values max-action actions)
    (if (eq? values null)
        max-action ; return when list is empty
        (if (> max-value (car values))
            (max-value-pos-helper max-value (cdr values) max-action (cdr actions))
            (max-value-pos-helper (car values) (cdr values) (car actions) (cdr actions)))))

  (max-value-pos-helper -100000 values 0 actions))


; returns the best action THIS IS THE TOP LEVEL FUNCTION FOR SERACHING
(define (get-best-action state columns)
  (define actions (get-actions state columns))
  (max-value-position (get-min-values state actions 0 columns) actions))

; this function handles make a move based on player input
(define (human-play state rows columns)
  (displayln "Please insert your input")
  (when (< (drop-piece state (string->number (read-line)) rows columns) 0)
      (let ()
        (displayln "Invalid move, pick a different column.")
        (human-play state rows columns))))

  