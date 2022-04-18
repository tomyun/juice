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
    (define j (kuten->sjis `(,(+ k (quotient (+ (sub1 t) i) 94))
                             ,(add1 (remainder (+ (sub1 t) i) 94)))))
    (charset!! j c))
  '())
(define (charset** k t . l) (apply charset* k t (flatten (map string->list l))))
(define (charset*** k t n c) (charset** k t (make-string n c)))
(define (charset-has-sjis? j) (hash-has-key? charset-sjis->char j))
(define (charset-has-char? c) (hash-has-key? charset-char->sjis c))
(define (charset-ref-sjis j) (hash-ref charset-sjis->char j))
(define (charset-ref-char c) (hash-ref charset-char->sjis c))

(define (char-sjis->char-utf8 c)
  (integer-sjis->char-utf8 (char->integer c)))

(define (integer-sjis->char-utf8 i)
  (define b (integer->integer-bytes i 2 #f #t))
  (read-char (reencode-input-port (open-input-bytes b) "sjis" (bytes) #t)))

(define (sjis->char-utf8 j)
  (integer-sjis->char-utf8 (sjis->integer j)))

(define (char-utf8->char-sjis c)
  (integer->integer-bytes (char-utf8->integer-sjis c) 2 #f #t))

(define (char-utf8->integer-sjis c)
  (define j (char-utf8->sjis c))
  (match-define `(,j1 ,j2) j)
  (+ (* j1 #x100) j2))

(define (char-utf8->sjis c)
  (let* ([s  (string c)]
         [n8 (string-utf-8-length s)]
         [b8 (string->bytes/utf-8 s)]
         [b  (make-bytes 2)]
         [t  (bytes-open-converter "utf-8" "sjis")])
      ;; avoid iconv issue: https://github.com/racket/racket/issues/3876
      (bytes-convert t b8 0 n8 b 0 2)
      (bytes-convert-end t b)
      (bytes-close-converter t)
      (bytes->list b)))

(define (sjis->char j)
  (if (charset-has-sjis? j)
    (charset-ref-sjis j)
    (let ([c (sjis->char-utf8 j)])
      (if (or (sjis-irregular? j)
              (sjis-nonstandard? j)
              (eof-object? c))
        #f
        c))))

(define (char->sjis c)
  (if (charset-has-char? c)
    (charset-ref-char c)
    (let ([j (char-utf8->sjis c)])
      (if (sjis-regular? j) j (error (format "SJIS decoding error: ~v => ~a" c j))))))

(define (sjis->integer j) (word->integer j))
(define (integer->sjis i) (integer->word i))

(define (kuten->sjis kt)
  (match-define `(,k ,t) kt)
  (define j1 (cond [(<=  1 k 62) (floor (/ (+ k 257) 2))]
                   [(<= 63 k 94) (floor (/ (+ k 385) 2))]))
  (define j2 (cond [(even? k)    (+ t 158)]
                   [(<=  1 t 63) (+ t 63)]
                   [(<= 64 t 94) (+ t 64)]))
  `(,j1 ,j2))

(define (sjis->kuten j)
  (match-define `(,j1 ,j2) j)
  (define i (if (<= j2 #x9E) 0 1))
  (define k (+ i (cond [(<= #x81 j1 #x9F)                 (- (* j1 2) 257)]
                       [(<= #xE0 j1 #xEF)                 (- (* j1 2) 385)]
                       [else                              #f])))
  (define t      (cond [(and (odd? k)  (<= #x40 j2 #x7E)) (- j2 63)]
                       [(and (odd? k)  (<= #x80 j2 #x9E)) (- j2 64)]
                       [(and (even? k) (<= #x9F j2 #xFC)) (- j2 158)]
                       [else                              #f]))
  `(,k ,t))

(define (kuten->jis kt)
  (match-define `(,k ,t) kt)
  `(,(+ k #x20) ,(+ t #x20)))

(define (jis->kuten j)
  (match-define `(,j1 ,j2) j)
  `(,(- j1 #x20) ,(- j2 #x20)))

(define (jis->integer j) (word->integer j))
(define (integer->jis i) (integer->word i))

(define (word->integer l)
  (match-define `(,c1 ,c2) l)
  (+ (arithmetic-shift c1 8) c2))

(define (integer->word i)
  (define c1 (arithmetic-shift (bitwise-and i #xFF00) -8))
  (define c2 (bitwise-and i #x00FF))
  `(,c1 ,c2))

;;HACK: check if SJIS code pointing to ASCII chars in section 9 - 15 (PC-98 exclusive)
(define (sjis-nonstandard? j)
  (match-define `(,k ,t) (sjis->kuten j))
  (<= 9 k 15))

(define (sjis-regular? j)
  (match-define `(,j1 ,j2) j)
  (and (or (<= #x81 j1 #x9F)
           (<= #xE0 j1 #xEF))
       (or (<= #x40 j2 #x7E)
           (<= #x80 j2 #x9E)
           (<= #x9F j2 #xFC))))
(define (sjis-irregular? j) (not (sjis-regular? j)))
 
(define (char-space c) (cfg:char-space c))
(define (char-newline c) (cfg:char-newline c)  (charset!! (char->sjis c) #\newline))
(define (char-cotinue c) (cfg:char-continue c) (charset!! (char->sjis c) #\tab))
(define (char-break c)   (cfg:char-break c)    (charset!! (char->sjis c) #\backspace))
(define (fontwidth w) (cfg:fontwidth w))

(provide charset
         charset*
         charset**
         sjis->char
         char->sjis
         sjis->integer
         integer->sjis
         kuten->sjis
         sjis->kuten
         kuten->jis
         jis->kuten
         jis->integer
         integer->jis
         char-space
         char-newline
         fontwidth)
