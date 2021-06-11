#lang racket

(require bitsyntax)

(define (load-bytes path) (port->bytes (open-input-file path)))
;(define example-mes (load-bytes "START.MES"))
;(define example-mes (load-bytes "MYHOUS.MES"))
;(define example-mes (load-bytes "YUI.MES"))

(define (bit-dict b)
  (define l (bytes->list (bit-string->bytes b)))
  (define (f l)
    (flatten (match l
              [(list a b c ...) (list (bytes a b) (f c))]
              [_ '()])))
  (define (g b) (read-char (reencode-input-port (open-input-bytes b) "sjis")))
  (map g (f l)))
;(define (bit-code b) (bit-string->bytes b))
(define (bit-code b) (bytes->string/latin-1 (bit-string->bytes b)))

(define (open-mes-bytes b)
  (bit-string-case b
   ([(offset :: integer little-endian bytes 2)
     (dict :: binary bytes (- offset 2))
     (code :: binary)]
    (list 'MES (bit-dict dict) (bit-code code)))))

(define (open-mes path) (open-mes-bytes (load-bytes path)))

;(define example-bit (open-mes-bytes example-mes))

(provide open-mes-bytes
         open-mes)
