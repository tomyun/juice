#lang racket/base

(require racket/file)
(require racket/match)

(require "mes-config.rkt")

(require (prefix-in ai1: "engine/ai1/mes-loader.rkt"))

(require (prefix-in ai5: "engine/ai5/mes-loader.rkt"))
(require (prefix-in ai5: "engine/ai5/mes-builder.rkt"))

(define (set-engine e)
  (cfg:engine
    (match e
      ['AI2    'AI1]
      ['AI4    'AI5]
      ['AI5X   (cfg:dictbase #xD0)
               (cfg:extraop  #t)
               'AI5]
      [a       a])))

(define (load-mes path)
  (match (cfg:engine)
    ['AI1 (ai1:load-mes path)]
    ['AI5 (ai5:load-mes path)]))

(define (build-mes path)
  (define src (file->value path))
  (define engine
    (match src
      [`(mes (meta (engine ,e) ,m ..) ,r ..) e]
      [a                                     (cfg:engine)]))
  (match engine
    ['AI5 (ai5:build-mes path)]))

(provide set-engine
         load-mes
         build-mes)
