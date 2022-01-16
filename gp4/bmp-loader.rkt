#lang racket/base

(require racket/match)
(require racket/path)

(require "bmp-opener.rkt")
(require "pic-struct.rkt")

(define (load-bmp path)
  (define f (open-bmp path))
  (match-define `(BMP ,w ,h ,pal ,data) f)
  (define n (file-name-from-path path))
  (define m (regexp-match #px"@(\\d+),(\\d+)." n))
  (unless m
    (error (format "Origin `@x,y` not specified in the filename: ~a" n)))
  (match-define `(,x ,y) (map string->number (cdr m)))
  (pic x y
       w h
       pal
       data))

;(define files '("0125_2.GP4" "0125D.GP4" "0125E.GP4" "0380.GP4" "0380B.GP4" "0390.GP4"))

(provide load-bmp)
