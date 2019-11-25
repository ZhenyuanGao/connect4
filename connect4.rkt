#lang slideshow
(require racket/class
         racket/gui/base)
(require (prefix-in 2dtp: 2htdp/image))
(require "Connect_four_window.rkt")
(require dyoo-while-loop)
;(require "connect4ai.rkt")

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
(define (check-dir board turn old-position columns get-next-pos)
  (define position (get-next-pos old-position columns))

  (cond
    ; return if continuing means going off of the board
    [(or (and (equal? (modulo position columns) 0) ; check if piece is at left border
              (equal? (modulo (get-next-pos position columns) columns) (- columns 1))) ; check if the next piece in the direction would pass border
         (and (equal? (modulo position columns) (- columns 1)) ; check if piece is at right border
              (equal? (modulo (get-next-pos position columns) columns) 0)) ; check if the next piece in the direction would pass border
         (> position (- (vector-length board) 1))) 0] ; pass bottom border
    ; if piece matches, continue in the same direction
    [(equal? (vector-ref board position) turn) (+ (check-dir board turn position columns get-next-pos) 1)]
    ; no match
    [else 0]))


; this function checks to see if someone has gotten a connect4
; returns true or false
(define (check-win state position columns)
  (define board (send state get-board))
  (define turn (send state get-turn))

  (cond
    [(equal? position (- (vector-length board) 3)) #f] ; no connect4 found after reaching the end of the board (last three don't need to be checked)
    [(equal? (vector-ref board position) turn) (cond ; if selected position matches player's piece, check for connect4 in the directions around it that haven't been explored
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p (- n 1)))) 1) 3) #t] ; check down and left
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p n))) 1) 3) #t] ; check down
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p (+ n 1)))) 1) 3) #t] ; check down and right
                                                 [(> (+ (check-dir board turn position columns (lambda (p n) (+ p 1))) 1) 3) #t] ; check right
                                                 [else (check-win state (+ position 1) columns)])] ; no connect4 found from this position, go to next position
    [else (check-win state (+ position 1) columns)])) ; if selected position doesn't have player's piece, go to next position

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