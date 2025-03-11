#lang racket

; A chess-color is one of:
; 'White
; 'Black

(define (chess-color-temp color)
  (...
    (cond
      [(equal? color 'White) ...]
      [(equal? color 'Black) ...])))

; A chess-type is one of:
; 'Pawn
; 'Knight
; 'Bishop
; 'Rook
; 'Queen
; 'King

(define (chess-type-temp type)
  (...
    (cond
      [(equal? type 'Pawn) ...]
      [(equal? type 'Knight) ...]
      [(equal? type 'Bishop) ...]
      [(equal? type 'Rook) ...]
      [(equal? type 'Queen) ...]
      [(equal? type 'King) ...])))

; A chesspiece is a (make-piece chess-color chess-type)
(define-struct chesspiece (color type))

(define (chesspiece-temp piece)
  (...
    (chesspiece-color-temp (chesspiece-color piece))
    (chesspiece-type-temp (chesspiece-type piece))))

; A chess-square is one of:
; null
; chesspiece

(define (chess-square-temp square)
  (...
    (cond
      [(null? square) ...]
      [(chesspiece? square)
       (...
         (chesspiece-temp square))])))

; A chess-row is a (list chess-square ...) of 8 chess-squares

; A chess-board is a (list chess-row ...) of 8 chess-rows

; 
; chess-board:
;
;     c0  c1  c2  c3  c4  c5  c6  c7
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

