#lang racket/base

(require racket/match)

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

(define (sjis k t)
  (define s1 (cond [(<=  1 k 62) (floor (/ (+ k 257) 2))]
                   [(<= 63 k 94) (floor (/ (+ k 385) 2))]))
  (define s2 (cond [(even? k)    (+ t 158)]
                   [(<=  1 t 63) (+ t 63)]
                   [(<= 64 t 94) (+ t 64)]))
  `(,s1 ,s2))

(define (jis s1 s2)
  (define i (if (<= s2 158) 0 1))
  (define k (+ i (cond [(<= 129 s1 159) (- (* s1 2) 257)]
                       [(<= 224 s1 239) (- (* s1 2) 385)])))
  (define t (cond [(even? k)            (- s2 158)]
                  [(<= s2 126)          (- s2 63)]
                  [(<= s2 158)          (- s2 64)]))
  `(,k ,t))

(provide char->sjis
         sjis->integer
         integer->sjis
         sjis
         jis)
