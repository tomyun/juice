#lang racket/base

(require racket/file)
(require racket/match)

(require "mes-config.rkt")

(require (prefix-in ai1: "engine/ai1/mes-loader.rkt"))
(require (prefix-in ai1: "engine/ai1/mes-builder.rkt"))

(require (prefix-in ai5: "engine/ai5/mes-loader.rkt"))
(require (prefix-in ai5: "engine/ai5/mes-builder.rkt"))

(define (load-mes path)
  (match (cfg:engine)
    ['AI1 (ai1:load-mes path)]
    ['AI5 (ai5:load-mes path)]))

(define (build-mes path)
  (define src (file->value path))
  (define engine
    (match src
      [`(mes (meta (engine ',e) ,m ...) ,r ...) e]
      [a                                       (cfg:engine)]))
  (match engine
    ['AI1 (ai1:build-mes path)]
    ['AI5 (ai5:build-mes path)]))

(provide load-mes
         build-mes)
