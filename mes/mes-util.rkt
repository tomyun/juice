#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require racket/port)

(define-namespace-anchor nsa)

(define charset-sjis->char (make-hash))
(define charset-char->sjis (make-hash))
(define (charset-reset)
  (set! charset-sjis->char (make-hash))
  (set! charset-char->sjis (make-hash)))
(define (charset-add k t . l)
  (for ([c l]
        [i (range (length l))])
    (define j (sjis (+ k (quotient (+ (sub1 t) i) 94))
                    (add1 (remainder (+ (sub1 t) i) 94))))
    (hash-set! charset-sjis->char j c)
    (hash-set! charset-char->sjis c j))
  '())
(define (charset-has-sjis? j) (hash-has-key? charset-sjis->char j))
(define (charset-has-char? c) (hash-has-key? charset-char->sjis c))
(define (charset-ref-sjis j) (hash-ref charset-sjis->char j))
(define (charset-ref-char c) (hash-ref charset-char->sjis c))
(define (charset-include f)
  (define ns (namespace-anchor->namespace nsa))
  (eval `(begin ,@(file->list f)) ns))
(define (charset k t . l) (apply charset-add k t l))

(define (sjis->char j)
  (if (charset-has-sjis? j)
    (charset-ref-sjis j)
    (let ([b (integer->integer-bytes (sjis->integer j) 2 #f #t)])
      (read-char (reencode-input-port (open-input-bytes b) "sjis" (bytes) #t)))))

(define (char->sjis c)
  (if (charset-has-char? c)
    (charset-ref-char c)
    (let* ([s  (string c)]
           [n8 (string-utf-8-length s)]
           [b8 (string->bytes/utf-8 s)]
           [b  (make-bytes 2)]
           [t  (bytes-open-converter "utf-8" "sjis")])
      ;; avoid iconv issue: https://github.com/racket/racket/issues/3876
      (bytes-convert t b8 0 n8 b 0 2)
      (bytes-convert-end t b)
      (bytes-close-converter t)
      (bytes->list b))))

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

(provide charset-reset
         charset-add
         charset-include
         charset
         sjis->char
         char->sjis
         sjis->integer
         integer->sjis
         sjis
         jis)
