#lang racket

(require racket/draw)

(require "bmp-common.rkt")

(define (blit w h X Y)
  (define dst (out))
  (for* ([x X]
         [y Y])
    (send src get-argb-pixels x y w h buf)
    (send dst set-argb-pixels x y w h buf)))

(define (init)
  ;; copy half-width glyphs in the first line
  (let ([w  8]
        [h 16])
    (blit w
          h
          (range 0 W w)
          (range 0 h h)))

  ;; copy full-width glyphs in the remaining lines
  (let ([w 16]
        [h 16])
    (blit w
          h
          (range 0 W w)
          (range h H h))))

;(init)

(provide init)
