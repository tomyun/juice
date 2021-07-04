#lang racket/base

(require racket/match)

(require "mes-config.rkt")

(require (prefix-in ai5: "engine/ai5/mes-loader.rkt"))
(require (prefix-in ai5: "engine/ai5/mes-builder.rkt"))

(define (load-mes path)
  (match (cfg:engine)
    ['ai5 (ai5:load-mes path)]))

(define (compile-mes path)
  (match (cfg:engine)
    ['ai5 (ai5:compile-mes path)]))

(provide load-mes
         compile-mes)
