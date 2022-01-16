#lang racket/base

(require racket/match)

(require "gp4-opener.rkt")
(require "gp4-parser.rkt")
(require "gp4-struct.rkt")
(require "pic-struct.rkt")

;;; Instructions

(define (move! g)
  (and (gp4-move! g) (gp4-reset-table! g)) g)

(define (draw! g . v)
  (define x (gp4-x g))
  (define y (gp4-y g))
  (for ([ix (in-range (length v))]
        [p v])
    (define c (gp4-poke-table! g p))
    (gp4-poke! g (+ x ix) y c))
  (move! g))

(define (copy! g dx dy h)
  (define ox (* 4 dx))
  (define oy dy)
  (for ([_ (in-range h)])
    (define x (gp4-x g))
    (define y (gp4-y g))
    (define sx (+ x ox))
    (define sy (+ y oy))
    (for ([ix (in-range 4)])
      (define c (gp4-peek g (+ sx ix) sy))
      (gp4-poke! g (+ x ix) y c))
    (move! g)))

;; File

(define (parse-gp4 path)
  (define f (open-gp4 path))
  (match-define `(GP4 ,x0 ,y0 ,w ,h ,pal ,raw) f)
  (parse-result <gp4> raw))

(define-namespace-anchor nsa)

(define (load-gp4 path)
  (define f (open-gp4 path))
  (match-define `(GP4 ,x0 ,y0 ,w ,h ,pal ,raw) f)
  (define g (create-gp4 x0 y0 w h pal))
  (define r (parse-result <gp4> raw))
  (define ns (namespace-anchor->namespace nsa))
  ((eval r ns) g)
  (pic
   (gp4-x0 g) (gp4-y0 g)
   (gp4-w g) (gp4-h g)
   (gp4-pal g)
   (gp4-data* g)))

(provide load-gp4)
