#lang racket

(require racket/draw)

(require "bmp-common.rkt")
(require "bmp-charset.rkt")

(define w 16)
(define h 16)
(define buf (make-bytes (* w h 4)))

(define (copy k0 t0 k1 t1)
  (define dst (out))
  (define (x k) (* k w))
  (define (y t) (* (+ t 32) h))
  (send src get-argb-pixels (x k0) (y t0) w h buf)
  (send dst set-argb-pixels (x k1) (y t1) w h buf))

(define (copy* k0 t0 k1 t1 n)
  (for ([i (range n)])
    (copy (nk k0 t0 i)
          (nt t0 i)
          (nk k1 t1 i)
          (nt t1 i))))

;(copy 9 1 85 1)
;(copy* 9 1 85 1 94)

(provide copy
         copy*)
