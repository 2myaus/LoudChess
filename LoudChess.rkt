#lang racket

; A chess-color is one of:
; 'White
; 'Black

; (define (chess-color-temp color)
;   (...
;     (cond
;       [(equal? color 'White) ...]
;       [(equal? color 'Black) ...])))


; A chess-type is one of:
; 'Pawn
; 'Knight
; 'Bishop
; 'Rook
; 'Queen
; 'King

; (define (chess-type-temp type)
;   (...
;     (cond
;       [(equal? type 'Pawn) ...]
;       [(equal? type 'Knight) ...]
;       [(equal? type 'Bishop) ...]
;       [(equal? type 'Rook) ...]
;       [(equal? type 'Queen) ...]
;       [(equal? type 'King) ...])))


; A chesspiece is a (make-chesspiece chess-color chess-type)
; It represents an individual piece on the board

(define-struct chesspiece (color type) #:transparent)

; (define (chesspiece-temp piece)
;   (...
;     (chesspiece-color-temp (chesspiece-color piece))
;     (chesspiece-type-temp (chesspiece-type piece))))


; A chess-square is a (make-chess-square number number)
; It represents a location on the chessboard

(define-struct chess-square (row col))


; A chess-move is a (make-chess-move chess-square chess-square chess-type)
; It represents a move as origin square to destination square,
; with promotion representing the chess-type that a pawn promotes to
; upon reaching the last rank, otherwise it is null

(define-struct chess-move (from to promotion))


; A chess-square-occupier is one of:
; null
; chesspiece

; (define (chess-square-occupier-temp square)
;   (...
;     (cond
;       [(null? square) ...]
;       [(chesspiece? square)
;        (...
;          (chesspiece-temp square))])))


; A chess-row is a (list chess-square-occupier ...) of 8 chess-square-occupiers


; A chess-board is a (list chess-row ...) of 8 chess-rows

; 
; chess-board:
;
;     s0  s1  s2  s3  s4  s5  s6  s7
;   --------------------------------
; r7| a8  b8  c8  d8  e8  f8  g8  h8
;   |
; r6| a7  b7  c7  d7  e7  f7  g7  h7
;   |
; r5| a6  b6  c6  d6  e6  f6  g6  h6
;   |
; r4| a5  b5  c5  d5  e5  f5  g5  h5
;   |
; r3| a4  b4  c4  d4  e4  f4  g4  h4
;   |
; r2| a3  b3  c3  d3  e3  f3  g3  h3
;   |
; r1| a2  b2  c2  d2  e2  f2  g2  h2
;   |
; r0| a1  b1  c1  d1  e1  f1  g1  h1

; A chess-position is a (make-chess-position chessboard chess-color chess-square number boolean boolean boolean boolean boolean boolean)
; This represents a complete chess position,
; to-play is a chess-color representing whose turn it is
; passant-square is the square *behind* a pawn which moved 2 spaces. This should be null unless a pawn moved 2 spaces on the last turn.
; nonpawn-moves represents the number of concurrent non-pawn moves (for the 50 move rule)
; white-king-moved says whether the whiet king has moved (for castling)
; etc..
;

(define-struct chess-position
               (board
                 to-play
                 passant-square
                 nonpawn-moves
                 white-king-moved
                 white-a-rook
                 white-h-rook
                 black-king-moved
                 black-a-rook
                 black-h-rook))


; square-in-bounds: chess-square -> boolean
; Returns #t if a square is within bounds and #f if it is not within bounds of a chessboard
; 
; (check-expect (square-in-bounds (make-chess-square 0 0)) #t)
; (check-expect (square-in-bounds (make-chess-square 7 7)) #t)
; (check-expect (square-in-bounds (make-chess-square 8 8)) #f)

(define (square-in-bounds square)
  (let [(row (chess-square-row square)) (col (chess-square-col square))]
    (and (>= row 0) (< row 8) (>= col 0) (< col 8))))


; offset-square: chess-square number number -> chess-square
; Returns a square offset from the given square by a certain
; number of rows and cols respectively
;
; (check-expect (offset-square (make-chess-square 1 2) 2 2) (make-chess-square 3 4))

(define (offset-square starting-square add-rows add-cols)
  (make-chess-square (+ (chess-square-row starting-square) add-rows) (+ (chess-square-col starting-square) add-cols)))


; get-board-occupant: chess-board chess-square -> chess-square-occupier
; Returns the chess-square-occupier of the given square on the given board
;
; (check-expect (get-board-occupant starting-board (make-chess-square 0 0)) (make-chesspiece 'White 'Rook))

(define (get-board-occupant board square)
  (list-ref (list-ref board (chess-square-row square)) (chess-square-col square)))


; get-pawn-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the pawn at the given square can move to
; Assumes that the piece is a pawn and the correct color to play

(define (get-pawn-moves-from-square position from-square)
  (let [(from-row (chess-square-row from-square))
        (from-col (chess-square-col from-square))
        (board (chess-position-board position))
        (to-play (chess-position-to-play position))
        (occupant (get-board-occupant (chess-position-board position) from-square))]
    (if (equal? (chesspiece-color occupant) 'White)
      ; White Pawn
      (let [(capture-L (offset-square from-square 1 1))
            (capture-R (offset-square from-square 1 -1)) 
            (push-1 (offset-square from-square 1 0))
            (push-2 (offset-square from-square 2 0)) ]
        (append
          (cond
            [(= from-row 7) null] ; Somehow the pawn is on the last rank :/
            [(not (null? (get-board-occupant board push-1))) null] ; The pawn is blocked

            [(and (<= from-row 1) (null? (get-board-occupant board push-2))) ; First Rank - Allow double advance
              (list
                (make-chess-move from-square push-1 null)
                (make-chess-move from-square push-2 null))]

            [(= from-row 6) ; Last Rank - Give promotion options
             (list
                (make-chess-move from-square push-1 'Knight)
                (make-chess-move from-square push-1 'Bishop)
                (make-chess-move from-square push-1 'Rook)
                (make-chess-move from-square push-1 'Queen))]

            [else
              (list
                 (make-chess-move from-square push-1 null))]

          )
          (append ; Captures
            (if (and
                  (square-in-bounds capture-L)
                  (not (null? get-board-occupant board capture-L))
                  (not (equal? (chesspiece-color (get-board-occupant board capture-L)) 'White)))
              (list (make-chess-move from-square capture-L))
              null
            )
            (if (and
                  (square-in-bounds capture-R)
                  (not (null? get-board-occupant board capture-R))
                  (not (equal? (chesspiece-color (get-board-occupant board capture-R)) 'White)))
              (list (make-chess-move from-square capture-R))
              null
            )
          )
        )
      )

      ; Black Pawn
      (let [(capture-L (offset-square from-square -1 1))
            (capture-R (offset-square from-square -1 -1)) 
            (push-1 (offset-square from-square -1 0))
            (push-2 (offset-square from-square -2 0)) ]
        (append
          (cond
            [(= from-row 0) null] ; Somehow the pawn is on the last rank :/
            [(not (null? (get-board-occupant board push-1))) null] ; The pawn is blocked

            [(and (<= from-row 6) (null? (get-board-occupant board push-2))) ; First Rank - Allow double advance
              (list
                (make-chess-move from-square push-1 null)
                (make-chess-move from-square push-2 null))]

            [(= from-row 1) ; Last Rank - Give promotion options
             (list
                (make-chess-move from-square push-1 'Knight)
                (make-chess-move from-square push-1 'Bishop)
                (make-chess-move from-square push-1 'Rook)
                (make-chess-move from-square push-1 'Queen))]

            [else
              (list
                 (make-chess-move from-square push-1 null))]

          )
          (append ; Captures
            (if (and
                  (square-in-bounds capture-L)
                  (not (null? get-board-occupant board capture-L))
                  (not (equal? (chesspiece-color (get-board-occupant board capture-L)) 'Black)))
              (list (make-chess-move from-square capture-L))
              null
            )
            (if (and
                  (square-in-bounds capture-R)
                  (not (null? get-board-occupant board capture-R))
                  (not (equal? (chesspiece-color (get-board-occupant board capture-R)) 'Black)))
              (list (make-chess-move from-square capture-R))
              null
            )
          )
        )
      )
    )
  )
)


; get-knight-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the knight at the given square can move to
; Assumes that the piece is a knight and the correct color to play

(define (get-knight-moves-from-square position from-square)
  (let [(from-row (chess-square-row from-square))
        (from-col (chess-square-col from-square))
        (board (chess-position-board position))
        (occupant (get-board-occupant (chess-position-board position) from-square))
        (to-play (chess-position-to-play position))

        (UP-left (offset-square from-square 2 -1))
        (UP-right (offset-square from-square 2 1))
        (RIGHT-up (offset-square from-square 1 2))
        (RIGHT-down (offset-square from-square -1 2))
        (DOWN-right (offset-square from-square -2 1))
        (DOWN-left (offset-square from-square -2 -1))
        (LEFT-down (offset-square from-square -1 -2))
        (LEFT-up (offset-square from-square 1 -2))]
    (append
      (if (and
            (square-in-bounds UP-left)
            (not (equal? (chesspiece-color (get-board-occupant board UP-left) to-play))))
        (list UP-left)
        null
      )
      (if (and
            (square-in-bounds UP-right)
            (not (equal? (chesspiece-color (get-board-occupant board UP-right) to-play))))
        (list UP-right)
        null
      )
      (if (and
            (square-in-bounds RIGHT-up)
            (not (equal? (chesspiece-color (get-board-occupant board RIGHT-up) to-play))))
        (list RIGHT-up)
        null
      )
      (if (and
            (square-in-bounds RIGHT-down)
            (not (equal? (chesspiece-color (get-board-occupant board RIGHT-down) to-play))))
        (list RIGHT-down)
        null
      )
      (if (and
            (square-in-bounds DOWN-right)
            (not (equal? (chesspiece-color (get-board-occupant board DOWN-right) to-play))))
        (list DOWN-right)
        null
      )
      (if (and
            (square-in-bounds DOWN-left)
            (not (equal? (chesspiece-color (get-board-occupant board DOWN-left) to-play))))
        (list DOWN-left)
        null
      )
      (if (and
            (square-in-bounds LEFT-down)
            (not (equal? (chesspiece-color (get-board-occupant board LEFT-down) to-play))))
        (list LEFT-down)
        null
      )
      (if (and
            (square-in-bounds LEFT-up)
            (not (equal? (chesspiece-color (get-board-occupant board LEFT-up) to-play))))
        (list LEFT-up)
        null
      )
    )
  )
)


; get-bishop-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the bishop at the given square can move to
; Assumes that the piece is a bishop and the correct color to play

(define (get-bishop-moves-from-square position from-square)
  (let [(from-row (chess-square-row from-square))
        (from-col (chess-square-col from-square))
        (board (chess-position-board position))
        (occupant (get-board-occupant (chess-position-board position) from-square))
        (to-play (chess-position-to-play position))]
    (define (get-squares-along at-square step-rows step-cols)
      (cond
        [(not (square-in-bounds at-square)) null] ; We are at the edge of the board
        [(null? (get-board-occupant at-square)) (cons at-square (get-squares-along (offset-square at-square step-rows step-cols)))] ; The square is not occupied
        [(equal? (chesspiece-color (get-board-occupant at-square)) to-play) null] ; The square is occupied by a piece of our color
        [else (list at-square)] ; The square is occupied by a piece of the other color
      )
    )

    (append
      (get-squares-along (offset-square from-square 1 1) 1 1)
      (get-squares-along (offset-square from-square -1 1) -1 1)
      (get-squares-along (offset-square from-square 1 -1) 1 -1)
      (get-squares-along (offset-square from-square -1 -1) -1 -1)) ))


; get-rook-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the rook at the given square can move to
; Assumes that the piece is a rook and the correct color to play

(define (get-rook-moves-from-square position from-square)
  (let [(from-row (chess-square-row from-square))
        (from-col (chess-square-col from-square))
        (board (chess-position-board position))
        (occupant (get-board-occupant (chess-position-board position) from-square))
        (to-play (chess-position-to-play position))]
    (define (get-squares-along at-square step-rows step-cols)
      (cond
        [(not (square-in-bounds at-square)) null] ; We are at the edge of the board
        [(null? (get-board-occupant at-square)) (cons at-square (get-squares-along (offset-square at-square step-rows step-cols)))] ; The square is not occupied
        [(equal? (chesspiece-color (get-board-occupant at-square)) to-play) null] ; The square is occupied by a piece of our color
        [else (list at-square)] ; The square is occupied by a piece of the other color
      )
    )

    (append
      (get-squares-along (offset-square from-square 1 0) 1 0)
      (get-squares-along (offset-square from-square -1 0) -1 0)
      (get-squares-along (offset-square from-square 0 -1) 0 -1)
      (get-squares-along (offset-square from-square 0 1) 0 1)) ))


; get-queen-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the queen at the given square can move to
; Assumes that the piece is a queen and the correct color to play

(define (get-queen-moves-from-square position from-square)
  (append
    (get-bishop-moves-from-square position from-square)
    (get-rook-moves-from-square position from-square)))


; get-moves-from-square: chess-position chess-square -> (list chess-move)
; Returns the chess-moves which the piece at the given square can move to

(define (get-moves-from-square position from-square)
  (let [(occupant (get-board-occupant (chess-position-board position) from-square))] (
    (cond
      [(null? occupant) null]
      [(not (equal? (chesspiece-color occupant) (chess-position-to-play position))) null]

      [(equal? (chesspiece-type occupant) 'Pawn) (get-pawn-moves-from-square position from-square)]
      [(equal? (chesspiece-type occupant) 'Knight) (get-knight-moves-from-square position from-square)]
      [(equal? (chesspiece-type occupant) 'Bishop) null]
      [(equal? (chesspiece-type occupant) 'Rook) null]
      [(equal? (chesspiece-type occupant) 'Queen) null]
      [(equal? (chesspiece-type occupant) 'King) null]) )))


(define starting-board
  (list
    (list
      (make-chesspiece 'White 'Rook) (make-chesspiece 'White 'Knight)
      (make-chesspiece 'White 'Bishop) (make-chesspiece 'White 'Queen)
      (make-chesspiece 'White 'King) (make-chesspiece 'White 'Bishop)
      (make-chesspiece 'White 'Knight) (make-chesspiece 'White 'Rook))
    (list
      (make-chesspiece 'White 'Pawn) (make-chesspiece 'White 'Pawn)
      (make-chesspiece 'White 'Pawn) (make-chesspiece 'White 'Pawn)
      (make-chesspiece 'White 'Pawn) (make-chesspiece 'White 'Pawn)
      (make-chesspiece 'White 'Pawn) (make-chesspiece 'White 'Pawn))
    (list null null null null null null null null)
    (list null null null null null null null null)
    (list null null null null null null null null)
    (list null null null null null null null null)
    (list
      (make-chesspiece 'Black 'Pawn) (make-chesspiece 'Black 'Pawn)
      (make-chesspiece 'Black 'Pawn) (make-chesspiece 'Black 'Pawn)
      (make-chesspiece 'Black 'Pawn) (make-chesspiece 'Black 'Pawn)
      (make-chesspiece 'Black 'Pawn) (make-chesspiece 'Black 'Pawn))
    (list
      (make-chesspiece 'Black 'Rook) (make-chesspiece 'Black 'Knight)
      (make-chesspiece 'Black 'Bishop) (make-chesspiece 'Black 'Queen)
      (make-chesspiece 'Black 'King) (make-chesspiece 'Black 'Bishop)
      (make-chesspiece 'Black 'Knight) (make-chesspiece 'Black 'Rook))))


(define starting-position
  (make-chess-position starting-board 'White null 0 #f #f #f #f #f #f))
