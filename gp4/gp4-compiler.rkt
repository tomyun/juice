#lang racket/base

(require racket/list)
(require racket/match)

(define (char->bit c)
  (match c
   [#\0 0]
   [#\1 1]
   [_   (error (format "[char->bit] unsupported char: ~a" c))]))

(define (bint i)
  (cond
   [(negative? i) (error (format "[bint] unsupported negative number: ~a" i))]
   [else          (map char->bit (string->list (number->string i 2)))]))

(define (num i n)
  (define l  (bint i))
  (define ni (length l))
  (cond
   [(< ni n) `(,@(make-list (- n ni) 0) ,@l)]
   [(> ni n) (error (format "[num] too large number: ~a (for ~a bits)" i n))]
   [else     l]))

(define (v i)
  `(,@(make-list i 1) 0))

(define (draw . V)
  `(0 ,@(flatten (map v V))))

(define (x1y y)
  `(0 ,@(num (+ y 8) 4)))

(define (x0y y)
  (define i (index-of '(-16 -8 -6 -5 -4 -3 -2 -1) y))
  (if i
    `(1 0 ,@(num i 3))
    (error (format "[x0y] unsupported offset: ~a" y))))

(define (x2y x y)
  `(1 1 ,@(v (- (+ x 2))) ,@(num (+ y 8) 4)))

(define (xy x y)
  (cond
   [(= x 0)  (x0y y)]
   [(= x -1) (x1y y)]
   [else     (x2y x y)]))

(define (h1 r)
  `(0 ,@(num (- r 2) 1)))

(define (h2 r)
  `(1 0 ,@(num (- r 4) 2)))

(define (h3 r)
  `(1 1 0 ,@(num (- r 8) 3)))

(define (h4 r)
  `(1 1 1 ,@(num (- r 16) 6)))

(define (h5 r)
  `(1 1 1 ,@(make-list 6 1) ,@(num (- r (+ 63 16)) 10)))

(define (h r)
  (cond
   [(<= 2  r 3)    (h1 r)]
   [(<= 4  r 7)    (h2 r)]
   [(<= 8  r 15)   (h3 r)]
   [(<= 16 r 78)   (h4 r)]
   [(<= 79 r 1102) (h5 r)]
   [else           (error (format "[h] unsupported repetition: ~a" r))]))

(define (copy x y r)
  `(1 ,@(xy x y) ,@(h r)))

(define (end)
  (make-list 7 0))

;;

(require bitsyntax)

(define I (bit-string (1 :: bits 1)))
(define O (bit-string (0 :: bits 1)))

(define (bit->bit-string b)
 (match b
   [0 O]
   [1 I]
   [_ (error (format "[bit->bit-string] unsupported bit: ~a" b))]))

(define-namespace-anchor nsa)

(define (encode r)
  (define ns (namespace-anchor->namespace nsa))
  (define l (flatten (eval r ns)))
  (bit-string->bytes (apply bit-string-append (map bit->bit-string l))))

(provide encode)
