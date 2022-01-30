#lang racket

(require "bmp.rkt")

(define (convert name k t [offset 0])
  (convert* name offset
    (λ (o n)
      ;(init)
      (jisfont o n 1 1))))

(define (convert* name offset block)
  (define fntname (format "font/~a.FNT" name))
  (define n (- (/ (file-size fntname) 32) offset))
  (displayln (format "~a ~a" fntname n))
  (parameterize ([out (new)]
                 [fnt (read-fnt fntname)])
      (block offset n)
      (save (format "font-~a.bmp" name))))

(define (convert-hannuri name)
  (convert* name 0
    (λ (o n)
      (define (f i k)
         (jisfont i               64 k        1)
         (jisfont (+ i 64 1)      30 k       65)
         (jisfont (+ i 64 1 30)   94 (+ k 1)  1))
      (define (g i k N)
         (for ([n N])
           (f (+ i (* n 189)) (+ k (* 2 n)))))
      (g 128 1 6)
      (jisfont 1262 2350 13 1)
      ;;FIXME: figure out offset
      (jisfont 3720 1121 39 1))))

(convert "nanpa1k" 1 1)
(convert "nanpa2k" 1 1)
(convert "nanpa2t" 1 1 128)
(convert-hannuri "nanpa2sk")
(convert-hannuri "JISHAN")
(convert "JIS" 1 1 112)
