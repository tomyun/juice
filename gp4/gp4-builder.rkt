#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/sequence)

(require 2htdp/image)
(require bitsyntax)

(require "pic-struct.rkt")
(require "gp4-struct.rkt")
(require "gp4-compiler.rkt")

(define (pic->gp4-raw p)
  (define L (pic-data p))
  (define w (pic-w p))
  (define h (pic-h p))
  (define (group l)
    (match l
     ['() '()]
     [_   `(,(take l w) ,@(group (drop l w)))]))
  (define (pack l)
    (match l
     ['() '()]
     [_   `(,(take l 4) ,@(pack (drop l 4)))]))
  (define (transpose l)
    (apply map list l))
  (define gL   (group L))       ; 400 x 640
  (define pgL  (map pack gL))   ; 400 x 160 x 4
  (define tpgL (transpose pgL)) ; 160 x 400 x 4
  (define (check a b)
    (length (take-common-prefix a b)))
  (define (clip n c x r)
    (filter (λ (o) (<= n (+ c o) x)) r))
  (define (candidates bx y)
    (append
     (for*/list ([oy (clip 0 y  h (reverse '(-16 -8 -6 -5 -4 -3 -2 -1)))])      ; -1 ~ -16
       `(0   ,oy))
     (for*/list ([ox (clip 0 bx w (reverse (sequence->list (in-range -4 0))))]  ; -1 ~ -4
                 [oy (clip 0 y  h (reverse (sequence->list (in-range -8 8))))]) ; 7 ~ -8
       `(,ox ,oy))))
  (define (slice l bx y oxy)
    (match-define `(,ox ,oy) oxy)
    (define sx (+ bx ox))
    (define sy (+ y oy))
    (drop (list-ref l sx) sy))
  (define (parse l)
    (define T (create-table))
    (define (colors! . C)
      (for/list ([c C])
        (define p (peek-table T c))
        (poke-table! T p)
        p))
    (define (reset-color!)
      (reset-table! T))
    (define r (for*/list ([bx (in-range (/ w 4))]
                          [op (in-generator
                                (let loop ([y 0])
                                  (when (< y h)
                                    (define t (drop (list-ref l bx) y))
                                    (define C (map (λ (c) `(,(check t (slice l bx y c)) ,c)) (candidates bx y)))
                                    (match-define `(,n ,c)
                                      (if (empty? C)
                                        '(1 #f)
                                        (argmax car C)))
                                    (cond
                                     ;[(< 1 n) (yield `(copy! g ,@c ,n))                    (loop (+ y n))]
                                     ;[else    (yield `(draw! g ,@(apply colors! (car t)))) (loop (+ y 1))]))
                                     [(< 1 n) (yield `(copy ,@c ,n))                    (loop (+ y n))]
                                     [else    (yield `(draw ,@(apply colors! (car t)))) (loop (+ y 1))]))
                                  (reset-color!)))])
                op))
    ;`(λ (g) (begin ,@r)))
    `(list ,@r (end)))
  (encode (parse tpgL)))
  ; (define g (create-gp4 (pic-x p) (pic-y p) w h (pic-pal p)))
  ; (set-gp4-buf! g buf)
  ; g)


(define (pic->gp4-code p)
  (define L (pic-data p))
  (define w (pic-w p))
  (define h (pic-h p))
  (define (group l)
    (match l
     ['() '()]
     [_   `(,(take l w) ,@(group (drop l w)))]))
  (define (pack l)
    (match l
     ['() '()]
     [_   `(,(take l 4) ,@(pack (drop l 4)))]))
  (define (transpose l)
    (apply map list l))
  (define gL   (group L))       ; 400 x 640
  (define pgL  (map pack gL))   ; 400 x 160 x 4
  (define tpgL (transpose pgL)) ; 160 x 400 x 4
  (define (check a b)
    (length (take-common-prefix a b)))
  (define (clip n c x r)
    (filter (λ (o) (<= n (+ c o) x)) r))
  (define (candidates bx y)
    (append
    ;  (for*/list ([oy (clip 0 y  h (reverse '(-16 -8 -6 -5 -4 -3 -2 -1)))])      ; -1 ~ -16
    ;    `(0   ,oy))
     ;(for*/list ([ox (clip 0 bx w (reverse (sequence->list (in-range -4 0))))]  ; -1 ~ -4
     ;            [oy (clip 0 y  h (reverse (sequence->list (in-range -8 8))))])) ; 7 ~ -8
     (for*/list ([ox (clip 0 bx w (reverse (sequence->list (in-range -7 0))))]  ; -1 ~ -7
                 [oy (clip 0 y  h (sequence->list (in-range -8 8)))]) ; 7 ~ -8
       `(,ox ,oy))
     (for*/list ([oy (clip 0 y  h '(-16 -8 -6 -5 -4 -3 -2 -1))])      ; -1 ~ -16
       `(0   ,oy))))
  (define (slice l bx y oxy)
    (match-define `(,ox ,oy) oxy)
    (define sx (+ bx ox))
    (define sy (+ y oy))
    (drop (list-ref l sx) sy))
  (define (parse l)
    (define T (create-table))
    (define (colors! . C)
      (for/list ([c C])
        (define p (peek-table T c))
        (poke-table! T p)
        p))
    (define (reset-color!)
      (reset-table! T))
    (define r (for*/list ([bx (in-range (/ w 4))]
                          [op (in-generator
                                (let loop ([y 0])
                                  (when (< y h)
                                    (define t (drop (list-ref l bx) y))
                                    (define C (map (λ (c) `(,(check t (slice l bx y c)) ,c)) (candidates bx y)))
                                    (match-define `(,n ,c)
                                      (if (empty? C)
                                        '(1 #f)
                                        (argmax car C)))
                                    (cond
                                     [(< 1 n) (yield `(copy! g ,@c ,n))                    (loop (+ y n))]
                                     [else    (yield `(draw! g ,@(apply colors! (car t)))) (loop (+ y 1))]))
                                  (reset-color!)))])
                op))
    `(λ (g) (begin ,@r)))
  (parse tpgL))






(define (pic->gp4-pal p)
  (define pal (pic-pal p))
  (define (color->bytes c)
    (let ([r (/ (color-red c) #x11)]
          [g (/ (color-green c) #x11)]
          [b (/ (color-blue c) #x11)])
      (bit-string->bytes
        (bit-string
         (g :: bits 4)
         (0 :: bits 1)
         (r :: bits 4)
         (0 :: bits 1)
         (b :: bits 4)
         (0 :: bits 1)
         (1 :: bits 1)))))
  (apply bytes-append (map color->bytes pal)))

(define (pic->gp4-bytes p)
  (let* ([x0  (pic-x p)]
         [y0  (pic-y p)]
         [w   (sub1 (pic-w p))]
         [h   (sub1 (pic-h p))]
         [pal (pic->gp4-pal p)]
         [raw (pic->gp4-raw p)]
         [len (bytes-length raw)])
    (bit-string->bytes
      (bit-string
       (x0  :: big-endian integer bytes 2)
       (y0  :: big-endian integer bytes 2)
       (w   :: big-endian integer bytes 2)
       (h   :: big-endian integer bytes 2)
       (pal :: binary bytes (* 2 16))
       (raw :: binary bytes len)))))

(define (pic->gp4-suffix p)
  (format "@~a,~a" (pic-x p) (pic-y p)))

;(with-output-to-file "test.gp4" #:exists 'replace (lambda () (write-bytes (pic->gp4-bytes p))))

(provide pic->gp4-code ;TODO: remove
         pic->gp4-bytes
         pic->gp4-suffix)
