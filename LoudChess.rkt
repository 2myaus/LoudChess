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


; A chess-move is a (make-chess-move chess-square chess-square)
; It represents a move as origin square to destination square

(define-struct chess-move (from to))


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


; get-moves-from-square: chess-position chess-square -> (list chess-square)
; Returns the chess-squares which the piece at the given square can move to

(define (get-moves-from-square position from-square)
  (let [(from-row (chess-square-row from-square))
        (from-col (chess-square-col from-square))
        (board (chess-position-board position))
        (occupant (get-board-occupant (chess-position-board position) from-square))](

      (cond
        [(null? occupant) null]
        [(not (equal? (chesspiece-color occupant) (chess-position-to-play position))) null]
        [(equal? (chesspiece-type occupant) 'Pawn) null]
        [(equal? (chesspiece-type occupant) 'Knight) null]
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
