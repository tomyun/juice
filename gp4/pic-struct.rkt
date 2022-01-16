#lang racket/base

(require racket/list)
(require racket/match)

(struct pic (x y w h pal data) #:mutable)

(define (create-pic x y w h pal)
  (pic x y
       w h
       pal
       (make-list (* w h) 0)))

(define (pic-x0 p) (pic-x p))
(define (pic-y0 p) (pic-y p))

(define (pic-x1 p) (sub1 (+ (pic-x0 p) (pic-w p))))
(define (pic-y1 p) (sub1 (+ (pic-y0 p) (pic-h p))))

(define (pic-pos p x y) (+ x (* y 640)))

(define (pic-data* p)
  (define l (pic-data p))
  (define-values (pt pb)
    (let* ([yt (pic-y p)]
           [yc (pic-h p)]
           [yb (- 400 yt yc)])
      (values (make-list (* yt 640) 0)
              (make-list (* yb 640) 0))))
  (define-values (pr pl)
    (let* ([xr (pic-x p)]
           [xc (pic-w p)]
           [xl (- 640 xr xc)])
      (values (make-list xr 0)
              (make-list xl 0))))
  (define w (pic-w p))
  (define (row l)
    (match l
      ['() '()]
      [_   `(,@pr ,@(take l w) ,@pl ,@(row (drop l w)))]))
  `(,@pt ,@(row l) ,@pb))

(define (pic-bytes p)
  (list->bytes (pic-data p)))

(define (pic-bytes* p)
  (list->bytes (pic-data* p)))

(provide (struct-out pic)
         create-pic
         pic-x0
         pic-y0
         pic-x1
         pic-y1
         pic-pos
         pic-data*
         pic-bytes
         pic-bytes*)
