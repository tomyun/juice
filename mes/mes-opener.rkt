#lang racket/base

(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(require file/sha1)

(require bitsyntax)

(define (load-bytes path) (port->bytes (open-input-file path) #:close? #t))

(define (bit-dict b)
  (define l (bytes->list (bit-string->bytes b)))
  (define (f l)
    (flatten (match l
              [`(,a ,b ,c ...) `(,(bytes a b) ,(f c))]
              [_               '()])))
  (define (g b) (read-char (reencode-input-port (open-input-bytes b) "sjis")))
  (map g (f l)))
(define (bit-code b) (bytes->string/latin-1 (bit-string->bytes b)))

(define (open-mes-bytes b)
  (bit-string-case b
   ([(offset :: integer little-endian bytes 2)
     (dict :: binary bytes (- offset 2))
     (code :: binary)]
    (list 'MES (bit-dict dict) (bit-code code)))))

(define (open-mes-snippet h)
  (bit-code (hex-string->bytes (string-replace h " " ""))))

(define (open-mes path) (open-mes-bytes (load-bytes path)))

(provide open-mes-bytes
         open-mes-snippet
         open-mes)
