#lang racket

(require parsack)
(require (only-in racket [string mk-string]))

(define-syntax-rule (@ p) (λ (x) (p x)))
(define-syntax-rule (:: p ...) (parser-seq p ...))
(define-syntax-rule (:~ p ...) (parser-one p ...))
(define-syntax-rule (:% p ...) (parser-compose p ...))
(define (optional p) (:% (<or> p (return '()))))

(define O (char #\0))
(define I (char #\1))
(define b (<or> O I))

(define (times n p)
  (cond
    [(positive? n) (:% (x <- p)
                       (xs <- (times (sub1 n) p))
                       (return (cons x xs)))]
    [(zero? n)     (return '())]))
(define (try-or . p) (apply <or> (map try p)))

(define (bint l)
  (match l
    [(? char? c) (bint `(,c))]
    [_           (string->number (apply mk-string l) 2)]))

(define v
  (:% (x <- (manyTill I O))
      (return (length x))))

(define draw
  (:% O
      (d <- (times 4 v))
      (return `(draw! g ,@d))))

(define x1y
  (:% O
      (y <- (times 4 b))               ; 0~15
      (return `(-1 ,(- (bint y) 8))))) ; (-1, -8~7)
(define x0y
  (:% I O
      (y <- (times 3 b))                                               ; 0~7
      (return `(0 ,(list-ref '(-16 -8 -6 -5 -4 -3 -2 -1) (bint y)))))) ; (0, [-16, -8, -6~-1])
(define x2y
  (:% I I
      (x <- v)                                   ; 0~
      (y <- (times 4 b))                         ; 0~15
      (return `(,(- (+ x 2)) ,(- (bint y) 8))))) ; (~-2, -8~7)
(define xy (try-or x1y x0y x2y))

(define h1
  (:% O
      (r <- b)                          ; 0~1
      (return (+ (bint r) 2))))         ; 2~3
(define h2
  (:% I O
      (r <- (times 2 b))                ; 0~3
      (return (+ (bint r) 4))))         ; 4~7
(define h3
  (:% I I O
      (r <- (times 3 b))                ; 0~7
      (return (+ (bint r) 8))))         ; 8~15
(define h4
  (:% I I I
      (r <- (times 6 b))                ; 0~62 (no 63)
      (return (+ (bint r) 16))))        ; 16~78 (no 79)
(define h5
  (:% I I I
      (times 6 I)
      (r <- (times 10 b))               ; 0~1023
      (return (+ (bint r) (+ 63 16))))) ; 79~1102
(define h (try-or h1 h2 h3 h5 h4))

(define copy
  (:% I
      (a <- xy)
      (b <- h)
      (return `(copy! g ,@a ,b))))

(define end
  (:% (times 15 (<or> O $eof))
      $eof
      (return `(end))))

(define command (try-or draw copy))

(define <gp4>
  (:% (l <- (manyTill command (try end)))
      (return `(λ (g) (begin ,@l)))))

(provide parse-result
         <gp4>)
