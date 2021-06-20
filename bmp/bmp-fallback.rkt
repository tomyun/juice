#lang racket

(require racket/draw)
(require file/sha1)

(require "bmp-common.rkt")
(require "bmp-charset.rkt")

(define 4x4-image (read-bitmap "assets/4x4-charset.png"))
(define 4x4-chars "abcdefghijklmnopqrstuvwxyz!?.()'\",:<>1234567890#■□█ ")
(define 4x4-dict  (make-hash (map cons (string->list 4x4-chars) (range (string-length 4x4-chars)))))

(define (blit-fallback c x y)
  (define dst (out))
  (define i (dict-ref 4x4-dict c))
  (send 4x4-image get-argb-pixels (* i 4) 0 4 4 buf)
  (send dst set-argb-pixels x y 4 4 buf))

(define (fallback k t)
  (define x (* 16 k))
  (define y (* 16 (+ t 32)))
  (for* ([dx (range 4)]
         [dy (range 4)])
    (blit-fallback #\  (+ x (* 4 dx)) (+ y (* 4 dy))))
  (define (f n) (~r n #:min-width 2 #:pad-string "0"))
  (define h (format "~a~a" (f k) (f t)))
  (blit-fallback (string-ref h 0) (+ x  0) (+ y  0))
  (blit-fallback (string-ref h 1) (+ x  4) (+ y  0))
  (blit-fallback (string-ref h 2) (+ x  8) (+ y  4))
  (blit-fallback (string-ref h 3) (+ x 12) (+ y  4))
  (define j (sjis->integer (sjis (nk k t) (nt t))))
  (define l (bytes->hex-string (integer->integer-bytes j 2 #f #t)))
  (blit-fallback (string-ref l 0) (+ x  0) (+ y  8))
  (blit-fallback (string-ref l 1) (+ x  4) (+ y  8))
  (blit-fallback (string-ref l 2) (+ x  8) (+ y 12))
  (blit-fallback (string-ref l 3) (+ x 12) (+ y 12)))

(define (fallback* k t n)
  (for ([i (range n)])
    (fallback (nk k t i) (nt t i))))
 
;(fallback* 15 1 (* 80 94))

(provide fallback
         fallback*)
