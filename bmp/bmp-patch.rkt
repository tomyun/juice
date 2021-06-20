#lang racket

(require racket/draw)

(require "bmp-common.rkt")

(define ref (read-bitmap "assets/unifont-13.0.06.bmp"))

(define w 16)
(define h 16)
(define buf (make-bytes (* w h 4)))

(define (blit-patch u1 u2 i j)
  (define dst (out))
  (let ([x (* (+ u2 2) w)]
        [y (* (+ u1 4) h)])
    (send ref get-argb-pixels x y w h buf))
  (let ([x (* i w)]
        [y (* j h)])
    (send dst set-argb-pixels x y w h buf)))

(define (patch c k t)
  (displayln (format "~a ~a ~a" c k t))
  (define u (char->integer c))
  (define u1 (arithmetic-shift (bitwise-and u #xFF00) -8))
  (define u2 (bitwise-and u #x00FF))
  (define i k)
  (define j (+ t 32))
  (blit-patch u1 u2 i j))

(define (patch* l k t)
  (define n (length l))
  (for ([c l]
        [i (range n)])
    (patch c
           (+ k (quotient (+ (sub1 t) i) 94))
           (add1 (remainder (+ (sub1 t) i) 94)))))

;(blit-patch #x00 #xc0 9 55)
;(patch #\u00c0 9 22)

;(include "patch-table.rkt")
;(include "patch-table-full.rkt")

(provide patch
         patch*)
