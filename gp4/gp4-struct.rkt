#lang racket/base

(require racket/list)

;;; Color Table

(define (rotate l p) (append (drop l p) (take l p)))

(struct table (data slot) #:mutable #:transparent)

(define (create-table)
  (table
   (for/vector ([p (in-inclusive-range 0 16)]) (rotate (range 16) p))
   16))

(define (peek-table t c)
  (define d (table-data t))
  (define s (table-slot t))
  (define r (vector-ref d s))
  (define p (index-of r c))
  p)

(define (poke-table! t p)
  (define d (table-data t))
  (define s (table-slot t))
  (define r (vector-ref d s))
  (define c (list-ref r p))
  (define r1 `(,c ,@(take r p) ,@(rest (drop r p))))
  (vector-set! d s r1)
  (set-table-slot! t c)
  c)

(define (reset-table! t) (set-table-slot! t 16))

(provide create-table
         peek-table
         poke-table!
         reset-table!)

;;; Structure

(struct gp4 (x0 y0 x1 y1 w h x y pal tbl buf) #:mutable)

(define (create-gp4 x0 y0 w h pal)
  (define x1 (sub1 (+ x0 w)))
  (define y1 (sub1 (+ y0 h)))
  (gp4
   x0 y0
   x1 y1
   w h
   x0 y0
   pal
   (create-table)
   (make-vector (* 640 400))))

(define (gp4-pos g x y) (+ x (* y 640)))

(define (gp4-data g)
  (vector->list (gp4-buf g)))

(define (gp4-data* g)
  (define b (list->bytes (gp4-data g)))
  (define x (gp4-x0 g))
  (define w (gp4-w g))
  (define l (for/list ([y (in-inclusive-range (gp4-y0 g) (gp4-y1 g))])
              (let* ([i0 (+ (* y 640) x)]
                     [i1 (+ i0 w)])
                (subbytes b i0 i1))))
  (bytes->list (apply bytes-append l)))

;;; Operations

(define (gp4-move! g)
  (define x (gp4-x g))
  (define y (gp4-y g))
  (define y0 (gp4-y0 g))
  (define y1 (gp4-y1 g))
  (cond [(< y y1)
         (set-gp4-y! g (add1 y))
         #f]
        [else
         (set-gp4-x! g (+ x 4))
         (set-gp4-y! g y0)
         #t]))

(define (gp4-peek g x y)
  (vector-ref (gp4-buf g) (gp4-pos g x y)))

(define (gp4-poke! g x y c)
  (vector-set! (gp4-buf g) (gp4-pos g x y) c))

(define (gp4-poke-table! g p)
  (poke-table! (gp4-tbl g) p))

(define (gp4-reset-table! g)
  (reset-table! (gp4-tbl g)))

(provide (struct-out gp4)
         create-gp4
         gp4-pos
         gp4-data
         gp4-data*
         gp4-move!
         gp4-peek
         gp4-poke!
         gp4-poke-table!
         gp4-reset-table!)
