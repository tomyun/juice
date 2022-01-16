#lang racket/base

(require 2htdp/image)
(require 2htdp/universe)

(require "pic-struct.rkt")

(define (pic->image p)
  (define P (pic-pal p))
  (define (color i) (list-ref P i))
  (define l (map color (pic-data* p)))
  (color-list->bitmap l 640 400))

(define (pic-show p)
  (define title
    (let ([x (pic-x p)]
          [y (pic-y p)]
          [w (pic-w p)]
          [h (pic-h p)])
      (format "(~a, ~a) (~a x ~a)" x y w h)))
  (define i (pic->image p))
  (big-bang '()
   (to-draw (λ _ i) 640 400)
   (name title)))

(provide pic->image
         pic-show)
