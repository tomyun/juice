#lang racket

(require racket/draw)

(require "bmp-common.rkt")
(require "bmp-charset.rkt")
(require "bmp-clear.rkt")

(define w 16)
(define h 16)

(define (char->ks c)
  (define s  (string c))
  (define n8 (string-utf-8-length s))
  (define b8 (string->bytes/utf-8 s))
  (define b  (make-bytes 2))
  (define t  (bytes-open-converter "utf-8" "cp949"))
  (bytes-convert t b8 0 n8 b 0 2)
  (bytes-convert-end t b)
  (bytes-close-converter t)
  (bytes->list b))

(define (ks->char l)
  (define nk (length l))
  (define bk (apply bytes l))
  (define n  (* nk 3))
  (define b  (make-bytes n))
  (define t  (bytes-open-converter "cp949" "utf8"))
  (bytes-convert t bk 0 nk b 0 n)
  (bytes-convert-end t b)
  (bytes-close-converter t)
  (string-ref (bytes->string/utf-8 b) 0))

(define (ks->integer l)
  (match-define `(,c1 ,c2) l)
  (+ (arithmetic-shift c1 8) c2))

(define (integer->ks i)
  (define c1 (arithmetic-shift (bitwise-and i #xFF00) -8))
  (define c2 (bitwise-and i #x00FF))
  `(,c1 ,c2))

(define (draw-korean c k t)
  (clear k t)
  (define dst (out))
  (define dc (send dst make-dc))
  (define f (make-font #:size 16
                       #:face "NeoDunggeunmo"
                       #:smoothing 'unsmoothed
                       #:size-in-pixels? #t
                       #:hinting 'unaligned))
  (send dc set-font f)
  (define x (* k w))
  (define y (* (+ t 32) h))
  (send dc draw-text (string c) x y))

(define (draw-korean* i N k0 t0)
  (for ([n (range N)])
    (define k (nk k0 t0 n))
    (define t (nt t0 n))
    (define c (ks->char (integer->ks (+ i n))))
    (displayln (format "n=~a c=~a k=~a t=~a" n c k t))
    (draw-korean c k t)))

(define (draw-korean/string s k0 t0)
  (for ([c s]
        [n (string-length s)])
    (define k (nk k0 t0 n))
    (define t (nt t0 n))
    (displayln (format "n=~a c=~a k=~a t=~a" n c k t))
    (draw-korean c k t)))

;(draw-korean #\가 45 1)
;(draw-korean* #xB0A1 2350 45 1)

(provide draw-korean
         draw-korean*
         draw-korean/string)
