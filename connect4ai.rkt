#lang racket
;(require "connect4.rkt")
(require racket/vector)
(require "game-functions.rkt")
(provide (all-defined-out))

(define greeting "Hello human.")
(define (change-greeting)
  (set! greeting "Goodbye human."))

(define (greet-ai)
  (print "Hello human."))

; 0 0 0 2 0 0 0 2 1 2 1 2 1 2 2 1 2 1 2 1 2 1 2 1 1 2 2 1 1 1 1 2 1 2 1 2 2 1 1 1 2 1

(define game-state (new state% [current-board (vector 0 0 0 2 0 0 0
                                                      2 1 2 1 2 1 2
                                                      2 1 2 1 2 1 2
                                                      1 2 1 1 2 2 1
                                                      1 1 1 2 1 2 1
                                                      2 2 1 1 1 2 1)] [current-turn 2] [current-status 1]))

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

(define (max-value-pos value-list)
  (define (max-value-pos-helper current-max value-list max-depth depth)
    (if (eq? value-list null)
        max-depth ; return when list is empty
        (if (> current-max (car value-list))
            (max-value-pos-helper current-max (cdr value-list) depth depth)
            (max-value-pos-helper (car value-list) (cdr value-list) max-depth depth))))

  (max-value-pos-helper -100 value-list 0 0))


; returns the best action or actions if tied
(define (get-best-action state columns)
  (print (get-actions state columns))
  (max-value-pos(get-min-values state (get-actions state columns) columns)))


(define (get-input state column rows columns)
  (when (< (drop-piece state column rows columns) 0)
      (let ()
        (print "try again")
        (get-input state (string->number (read-line)) rows columns))))

(define (gameloop state)
  (print (send game-state get-board))
  (get-input state (string->number (read-line)) 5 7)
  (when (check-win state 0 7) (print "Human wins!"))
  (send state change-turn)

  (print (send game-state get-board))
  (print(drop-piece state (get-best-action state 7) 5 7))
  (when (check-win state 0 7) (print "Computer wins!"))
  (send state change-turn)

  (gameloop state))

  