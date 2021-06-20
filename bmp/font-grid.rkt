#lang racket

(require "bmp.rkt")

(parameterize ([out (new)])
  (init)
  (fallback* 1 1 (* 94 94))
  (save "font-grid.bmp"))
