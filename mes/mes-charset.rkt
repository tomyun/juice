#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/runtime-path)
(require racket/string)

(require "mes-config.rkt")

(define-namespace-anchor nsa)

(define-runtime-path charset-collection "./charset/")
(define (charset-path s)
  (cond
    [(string-suffix? s ".rkt") s]
    [else                      (build-path charset-collection (format "_charset_~a.rkt" s))]))
(define (charset n)
  (charset-reset)
  (define ns (namespace-anchor->namespace nsa))
  (eval `(begin ,@(file->list (charset-path n))) ns))

(define charset-sjis->char (make-hash))
(define charset-char->sjis (make-hash))
(define (charset-reset)
  (set! charset-sjis->char (make-hash))
  (set! charset-char->sjis (make-hash)))
(define (charset! i c) (charset!! (integer->sjis i) c))
(define (charset!! j c)
  (hash-set! charset-sjis->char j c)
  (hash-set! charset-char->sjis c j))
(define (charset* k t . l)
  (for ([c l]
        [i (range (length l))])
    (define j (sjis (+ k (quotient (+ (sub1 t) i) 94))
                    (add1 (remainder (+ (sub1 t) i) 94))))
    (charset!! j c))
  '())
(define (charset** k t . l) (apply charset* k t (flatten (map string->list l))))
(define (charset*** k t n c) (charset** k t (make-string n c)))
(define (charset-has-sjis? j) (hash-has-key? charset-sjis->char j))
(define (charset-has-char? c) (hash-has-key? charset-char->sjis c))
(define (charset-ref-sjis j) (hash-ref charset-sjis->char j))
(define (charset-ref-char c) (hash-ref charset-char->sjis c))

(define (sjis->char j)
  (if (charset-has-sjis? j)
    (charset-ref-sjis j)
    (let* ([b (integer->integer-bytes (sjis->integer j) 2 #f #t)]
           [c (read-char (reencode-input-port (open-input-bytes b) "sjis" (bytes) #t))])
      (if (or (sjis-irregular? j)
              (sjis-nonstandard? j)
              (eof-object? c))
        #f
        c))))

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
  (define i (if (<= s2 #x9E) 0 1))
  (define k (+ i (cond [(<= #x81 s1 #x9F)                 (- (* s1 2) 257)]
                       [(<= #xE0 s1 #xEF)                 (- (* s1 2) 385)]
                       [else                              #f])))
  (define t      (cond [(and (odd? k)  (<= #x40 s2 #x7E)) (- s2 63)]
                       [(and (odd? k)  (<= #x80 s2 #x9E)) (- s2 64)]
                       [(and (even? k) (<= #x9F s2 #xFC)) (- s2 158)]
                       [else                              #f]))
  `(,k ,t))

;;HACK: check if SJIS code pointing to ASCII chars in section 9 - 15 (PC-98 exclusive)
(define (sjis-nonstandard? l)
  (match-define `(,k ,t) (apply jis l))
  (<= 9 k 15))

(define (sjis-regular? l)
  (match-define `(,s1 ,s2) l)
  (and (or (<= #x81 s1 #x9F)
           (<= #xE0 s1 #xEF))
       (or (<= #x40 s2 #x7E)
           (<= #x80 s2 #x9E)
           (<= #x9F s2 #xFC))))
(define (sjis-irregular? l) (not (sjis-regular? l)))
 
(define (charspc c) (cfg:charspc c))
(define (fontwidth w) (cfg:fontwidth w))

(provide charset
         charset*
         charset**
         sjis->char
         char->sjis
         sjis->integer
         integer->sjis
         sjis
         jis
         charspc
         fontwidth)
