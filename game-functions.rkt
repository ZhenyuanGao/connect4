#lang racket
(provide (all-defined-out))

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

    (define/public (get-piece position)
      (vector-ref board position))

    (define/public (update-board position value)
      (vector-set! board position value))

    ; functions related to turn
    (define/public (get-turn)
      turn)

    (define/public (change-turn)
      (if (equal? turn 1)
          (set! turn 2)
          (set! turn 1)))

    ; functions related to game status
    (define/public (get-status)
      status)

    (define/public (change-status value)
      (set! status value))))

(define (reset-board rows columns)
  (make-vector (* rows columns) 0))

; this function changes the turn
; return void
(define (change-turn state)
  (if (equal? (send state get-turn) 1) ; change turn
      (send state change-turn 2)
      (send state change-turn 1)))

; this function handles the dropping of a piece into the board
; the function returns the position in the board vector the piece was placed in if successful, otherwise
; it returns an error code
(define (drop-piece state column row columns)
  (define position (+ column (* row columns))) ; place on board to check

  ; base cases
  (cond
    [(or (> column columns) (< column 0)) -2] ; invalid column
    [(or (equal? (send state get-piece column) 1) (equal? (send state get-piece column) 2)) -1] ; row is full
    [(equal? (send state get-piece position) 0) ; empty spot
     (let ()
       (send state update-board position (send state get-turn)) ; place piece in empty spot
       position)] ; return position of placed piece
    [else (drop-piece state column (- row 1) columns)])) ; check the spot above

; this function counts the number of consecutive same colored pieces in a given direction
; returns the count of consecutive same colored pieces
(define (check-dir board turn prev-position columns get-next-pos)
  (define position (get-next-pos prev-position columns))

  #|(when (equal? (modulo prev-position columns) (- columns 1))
    (print "old position: ")
    (print prev-position)
    (print "position inside: ")
    (print position)
    (print "right border? ")
    (print (modulo prev-position columns))
    (print (equal? (modulo prev-position columns) (- columns 1)))
    (print "next pass right border? ")
    (print (modulo position columns))
    (print (equal? (modulo position columns) 0)))|#

  (cond
    ; return if continuing means going off of the board
    [(or (and (equal? (modulo prev-position columns) 0) ; check if previous piece is at left border
              (equal? (modulo position columns) (- columns 1))) ; check if the next piece in the direction would pass border
         (and (equal? (modulo prev-position columns) (- columns 1)) ; check if piece is at right border
              (equal? (modulo position columns) 0)) ; check if the next piece in the direction would pass border
         (> position (- (vector-length board) 1))) 0] ; pass bottom border
    ; if piece matches, continue in the same direction
    [(equal? (vector-ref board position) turn) (+ (check-dir board turn position columns get-next-pos) 1)]
    ; no match
    [else 0]))

; checks all possible moves and appends the valid moves to a list
(define (get-actions-helper state column-list)
  (cond
    [(eq? column-list null) '()] ; return on empty list
    [(equal? (send state get-piece (car column-list)) 0) (append (build-list 1 (lambda (x) (car column-list))) (get-actions-helper state (cdr column-list)))] ; add valid move to list 
    [else (get-actions-helper state (cdr column-list))])) ; conitnue on if move not valid

; returns a list of possible columns to drop a piece into
(define (get-actions state columns)
  (get-actions-helper state (build-list columns values)))


; this function checks to see if someone has gotten a connect4
; returns a number
;   0 - no one has won yet
;   1 - player has won
;   2 - player 2 has won
;   3 - game is a tie
(define (check-win state position columns)
  (define board (send state get-board))
  (define turn (send state get-turn))

  (cond
    [(equal? position (- (vector-length board) 3)) (if (eq? (get-actions state columns) null) 3 0)] ; return 3 if there isn't a win and aren't any moves, else return 0 since there isn't a win but are possible moves
    [(equal? (vector-ref board position) turn) (cond ; if selected position matches player's piece, check for connect4 in the directions around it that haven't been explored
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p (- n 1)))) 1) 3) (send state get-turn)] ; check down and left
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p n))) 1) 3) (send state get-turn)] ; check down
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p (+ n 1)))) 1) 3) (send state get-turn)] ; check down and right
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p 1))) 1) 3) (send state get-turn)] ; check right
                                                 [else (check-win state (+ position 1) columns)])] ; no connect4 found from this position, go to next position
    [else (check-win state (+ position 1) columns)])) ; if selected position doesn't have player's piece, go to next position