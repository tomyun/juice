#lang racket

(require racket/draw)

(require bitsyntax)

(require "bmp-common.rkt")
(require "bmp-charset.rkt")

(define w 16)
(define h 16)

(define (read-fnt path)
  (define n (file-size path))
  (define f (open-input-file path))
  (define b (read-bytes n f))
  (close-input-port f)
  b)
(define fnt (make-parameter (read-fnt "assets/JIS.FNT")))

(define (get-glyph i w h)
  (define u (/ (* w h) 8))
  (define i0 (* i u))
  (define i1 (+ i0 u))
  (subbytes (fnt) i0 i1))

(define (print-glyph i w h)
  (define g (get-glyph i w h))
  (define w/ (/ w 8))
  (for ([y (range h)])
    (for ([x (range w/)])
      (define b (bytes-ref g (+ x (* y w/))))
      (for ([i (range 8)])
        (define p (bitwise-and (arithmetic-shift b (add1 (- i 8))) #x01))
        (display (if (zero? p) " " "█"))))
    (displayln "")))

(define (blit i k t)
  (define dst (out))
  (define dx (* k w))
  (define dy (* (+ t 32) h))
  (define g (get-glyph i w h))
  (define w/ (/ w 8))
  (define I (bytes #xFF #x00 #x00 #x00))
  (define O (bytes #xFF #xFF #xFF #xFF))
  (for ([y (range h)])
    (for ([x (range w/)])
      (define b (bytes-ref g (+ x (* y w/))))
      (for ([i (range 8)])
        (define p (bitwise-and (arithmetic-shift b (add1 (- i 8))) #x01))
        (display (if (zero? p) " " "█"))
        (send dst set-argb-pixels
                  (+ (+ dx (* x 8) i))
                  (+ dy y)
                  1
                  1
                  (if (zero? p) O I))))
    (displayln "")))

(define (jisfont i N k0 t0)
  (for ([n (range N)])
    (define k (nk k0 t0 n))
    (define t (nt t0 n))
    (displayln (format "n=~a k=~a t=~a" n k t))
    (blit (+ i n) k t)))

;(jisfont 1804 2350 45 1)

(provide read-fnt
         fnt
         jisfont)
 