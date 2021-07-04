#lang racket/base

(require racket/file)
(require racket/match)

(require "mes-config.rkt")

(require (prefix-in ai1: "engine/ai1/mes-loader.rkt"))

(require (prefix-in ai5: "engine/ai5/mes-loader.rkt"))
(require (prefix-in ai5: "engine/ai5/mes-builder.rkt"))

(define (load-mes path)
  (match (cfg:engine)
    ['ai1 (ai1:load-mes path)]
    ['ai5 (ai5:load-mes path)]))

(define (compile-mes path)
  (define src (file->value path))
  (define engine
    (match src
      [`(mes (meta (engine ,e) ,m ..) ,r ..) e]
      [a                                     (cfg:engine)]))
  (match engine
    ['ai5 (ai5:compile-mes path)]))

(provide load-mes
         compile-mes)
