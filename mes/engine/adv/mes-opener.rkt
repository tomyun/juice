#lang racket/base

(require racket/port)
(require racket/string)

(require file/sha1)

(define (load-bytes path) (port->bytes (open-input-file path) #:close? #t))

(define (bit-code b) (bytes->string/latin-1 b))

(define (open-mes-bytes b)
  (list 'MES (bit-code b)))

(define (open-mes-snippet h)
  (bit-code (hex-string->bytes (string-normalize-spaces h #px"\\s+" ""))))

(define (open-mes path) (open-mes-bytes (load-bytes path)))

(provide open-mes-bytes
         open-mes-snippet
         open-mes)
