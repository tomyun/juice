#lang racket/base

(require racket/file)
(require racket/match)

(require "mes-config.rkt")

(require (prefix-in adv: "engine/adv/mes-loader.rkt"))
(require (prefix-in adv: "engine/adv/mes-builder.rkt"))

(require (prefix-in ai1: "engine/ai1/mes-loader.rkt"))
(require (prefix-in ai1: "engine/ai1/mes-builder.rkt"))

(require (prefix-in ai5: "engine/ai5/mes-loader.rkt"))
(require (prefix-in ai5: "engine/ai5/mes-builder.rkt"))

(define (load-mes path)
  (match (cfg:engine)
    ['ADV (adv:load-mes path)]
    ['AI1 (ai1:load-mes path)]
    ['AI5 (ai5:load-mes path)]))

(define (build-mes path)
  (define src (file->value path))
  (build-mes-source src))

(define (build-mes-source src)
  (define engine
    (match src
      [`(mes (meta (engine ',e) ,m ...) ,r ...) e]
      [a                                       (cfg:engine)]))
  (match engine
    ['ADV (adv:build-mes-source src)]
    ['AI1 (ai1:build-mes-source src)]
    ['AI5 (ai5:build-mes-source src)]))

(provide load-mes
         build-mes
         build-mes-source)
