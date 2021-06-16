#lang racket

(define (char->sjis c)
  ; avoid iconv issue: https://github.com/racket/racket/issues/3876
  (define s  (string c))
  (define n8 (string-utf-8-length s))
  (define b8 (string->bytes/utf-8 s))
  (define b  (make-bytes 2))
  (define t  (bytes-open-converter "utf-8" "shift_jisx0213"))
  (bytes-convert t b8 0 n8 b 0 2)
  (bytes-convert-end t b)
  (bytes-close-converter t)
  (bytes->list b))

(define (sjis->integer l)
  (match-define `(,c1 ,c2) l)
  (+ (arithmetic-shift c1 8) c2))

(define (integer->sjis i)
  (define c1 (arithmetic-shift (bitwise-and i #xFF00) -8))
  (define c2 (bitwise-and i #x00FF))
  `(,c1 ,c2))

(provide char->sjis
         sjis->integer
         integer->sjis)
