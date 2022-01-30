#lang racket

(define (nk k t [i 0]) (+ k (quotient (+ (sub1 t) i) 94)))
(define (nt t [i 0])   (add1 (remainder (+ (sub1 t) i) 94)))

(define (sjis k t)
  (define s1 (cond [(<=  1 k 62) (floor (/ (+ k 257) 2))]
                   [(<= 63 k 94) (floor (/ (+ k 385) 2))]))
  (define s2 (cond [(even? k)    (+ t 158)]
                   [(<=  1 t 63) (+ t 63)]
                   [(<= 64 t 94) (+ t 64)]))
  `(,s1 ,s2))

(define (sjis->integer l)
  (match-define `(,c1 ,c2) l)
  (+ (arithmetic-shift c1 8) c2))

(define (sjis-tbl l k t)
  (define n (length l))
  (for/list ([i (range n)])
    (sjis (nk k t i) (nt t i))))

(provide nk
         nt
         sjis
         sjis->integer
         sjis-tbl)
