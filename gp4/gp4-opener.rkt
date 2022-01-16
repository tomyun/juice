#lang racket

(require bitsyntax)
(require 2htdp/image)

(define (bit-string->gp4-pal b)
  (define l (bytes->list (bit-string->bytes b)))
  (define (combine l)
    (flatten
      (match l
       [`(,a ,b ,r ...) `(,(bytes a b) ,(combine r))]
       [_               '()])))
  (define (parse b)
    (define c (integer-bytes->integer b #f #t))
    (define (s i)
      (* (modulo (arithmetic-shift c i) 16) #x11))
    (let ([g (s -12)]
          [r (s -7)]
          [b (s -2)])
      (make-color r g b)))
  (map parse (combine l)))

(define (bit-string->gp4-raw b)
  (define l (bytes->list (bit-string->bytes b)))
  (define (: i)
    (~r #:base 2 #:min-width 8 #:pad-string "0" i))
  (string-join (map : l) ""))

(define (open-gp4-bytes b)
  (bit-string-case b
   ([(x   :: big-endian integer bytes 2)
     (y   :: big-endian integer bytes 2)
     (w   :: big-endian integer bytes 2)
     (h   :: big-endian integer bytes 2)
     (pal :: binary bytes (* 2 16))
     (raw :: binary)]
    (list 'GP4 x y
               (add1 w) (add1 h)
               (bit-string->gp4-pal pal)
               (bit-string->gp4-raw raw)))))

(define (load-bytes path) (port->bytes (open-input-file path)))
(define (open-gp4 path) (open-gp4-bytes (load-bytes path)))

(provide open-gp4-bytes
         open-gp4)
