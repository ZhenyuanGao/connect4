#lang racket

; board: the current game board
; turn: the player whose turn it is
; status: whether or not the game is still going or the game has ended
;(struct state (board turn status))

(define state%
  (class object%
    (init current-board current-turn current-status)

    (define board current-board)
    (define turn current-turn)
    (define status current-status)

    (super-new)

    ; functions related to board
    (define/public (get-board)
      board)

    (define/public (get-position position)
      (vector-ref board position))

    (define/public (update-board position value)
      (vector-set! board position value))

    ; functions related to turn
    (define/public (get-turn)
      turn)

    (define/public (change-turn player)
      (set! turn player))

    ; functions related to game status
    (define/public (get-status)
      status)

    (define/public (update-status value)
      (set! status value))))

(define (reset-board)
  (make-vector (* rows columns) 0))
  ;(vector 0 0 0 0 0 1 0
  ;        0 0 1 0 0 1 0
  ;        0 0 1 0 0 1 0
  ;        1 0 1 0 0 1 1
  ;        1 0 1 1 0 1 1
  ;        1 0 1 1 0 1 1))

(define (print-board board columns)
  (define length (vector-length board))
  (cond
    [(equal? length 0) 0] ; return when no more elements in vector
    [(< length columns) (print (vector->values board 0 length))] ; print remaing values (this shouldn't happen though)
    [else (let()
            (print board)
            (print (vector->values board 0 columns)) ; print a whole row
            (print-board (vector->values board columns (vector-length))))])) ; continue printing

; this function handles the dropping of a piece into the board
; the function returns the position in the board vector the piece was placed in if successful, otherwise
; it returns an error code
(define (drop-piece state column row columns)
  (define position (+ column (* row columns))) ; place on board to check
  ; base cases
  (cond
    [(or (> column columns) (< column 0)) -2] ; invalid column
    [(or (equal? (send state get-position column) 1) (equal? (send state get-position column) 2)) -1] ; row is full
    [(equal? (send state get-position position) 0) ; empty spot
     (let ()
       (send state update-board position (send state get-turn)) ; place piece in empty spot
       (if (equal? (send state get-turn) 1) ; change turn
           (send state change-turn 2)
           (send state change-turn 1))
       position)]
    [else (drop-piece state column (- row 1) columns)])) ; check the spot above

(define (check-win state position)
  (print 'hello))

(define rows 6)
(define columns 7)
(define game-state (new state% [current-board (reset-board)] [current-turn 1] [current-status 1]))

; game loop
;(define (game-loop))
