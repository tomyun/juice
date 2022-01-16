#lang racket/base

(require racket/list)
(require racket/match)

(require 2htdp/image)
(require bitsyntax)

(require "pic-struct.rkt")

(define (pic->pcx-pal p)
  (define (color->list c)
    `(,(color-red   c)
      ,(color-green c)
      ,(color-blue  c)))
  (list->bytes (flatten (map color->list (pic-pal p)))))

(define (pic->pcx-bpl p)
  (* (ceiling (/ (pic-w p) 16)) 2))

;; PCX format reference
;; https://moddingwiki.shikadi.net/wiki/PCX_Format
;; https://en.wikipedia.org/wiki/PCX
(define (pic->pcx-dat p)
  (define L (pic-data p))
  (define w (pic-w p))
  (define (pad l)
    (define n (length l))
    (if (< n 16)
      `(,@l ,@(make-list (- 16 n) 0))
      l))
  (define (scan l)
    (match l
     ['() '()]
     [_   `(,(pad (take l w)) ,@(scan (drop l w)))]))
  (define (extract l p)
    (let ([m (arithmetic-shift 1 p)])
      (map (λ (v) (arithmetic-shift (bitwise-and v m) (- p))) l)))
  (define (split l)
    (for/list ([p (in-range 4)]) (extract l p)))
  (define (list->byte l)
    (foldl (λ (a r) (+ a (arithmetic-shift r 1))) 0 l))
  (define (pack l)
    (match l
     ['() '()]
     [_   `(,(list->byte (take l 8)) ,@(pack (drop l 8)))]))
  (define (merge l)
    (match l
     ['()                   '()]
     [`(,a ,b ,c ,d ,r ...) `((,@(pack a) ,@(pack b) ,@(pack c) ,@(pack d)) ,@(merge r))]))
  (define (encode l)
    (match l
     [`('(,a ,n) ,b ,r ...) #:when (and (= a b) (< n 63)) (encode `('(,a ,(add1 n)) ,@r))]
     [`('(,a ,n) ,b ,r ...)                               `('(,a ,n) ,@(encode `(,b ,@r)))]
     [`(,a       ,b ,r ...) #:when (= a b)                (encode `('(,a 2) ,@r))]
     [`(,a       ,b ,r ...)                               `(,a       ,@(encode `(,b ,@r)))]
     [x                                                   x]))
  (define (store l)
    (match l
     [`('(,a ,n) ,r ...)                    `(,(+ #xC0 n) ,a ,@(store r))]
     [`(,a       ,r ...) #:when (>= a #xC0) `(,(+ #xC0 1) ,a ,@(store r))]
     [`(,a       ,r ...)                    `(,a             ,@(store r))]
     [x                                     x]))
  (define sL      (scan L))           ; 400 x 640
  (define ssL     (map split sL))     ; 400 x 4 x 640
  (define assL    (apply append ssL)) ; 1600 x 640
  (define massL   (merge assL))       ; 400 x (4 x 80)
  (define emassL  (map encode massL)) ; 400 x <=320
  (define semassL (map store emassL)) ; 400 x <=320?
  (list->bytes (flatten semassL)))

(define (pic->pcx-bytes p)
  (let* ([x0  (pic-x0 p)]
         [y0  (pic-y0 p)]
         [x1  (pic-x1 p)]
         [y1  (pic-y1 p)]
         [w   (pic-w p)]
         [y   (pic-h p)]
         [pal (pic->pcx-pal p)]
         [bpl (pic->pcx-bpl p)]
         [pad (make-bytes 54)]
         [dat (pic->pcx-dat p)]
         [len (bytes-length dat)])
    (bit-string->bytes
      (bit-string
       (#x0A :: bytes 1) ; PCX
       (#x05 :: bytes 1) ; PC Paintbrush version 3.0
       (#x01 :: bytes 1) ; run length encoding
       (1    :: integer bytes 1) ; number of bits per pixel in each plane
       (x0   :: little-endian integer bytes 2) ; minimum x coordinate of the image position
       (y0   :: little-endian integer bytes 2) ; minimum y coordinate of the image position
       (x1   :: little-endian integer bytes 2) ; maximum x coordinate of the image position
       (y1   :: little-endian integer bytes 2) ; maximum y coordinate of the image position
       (60   :: little-endian integer bytes 2) ; horizontal image resolution in DPI
       (60   :: little-endian integer bytes 2) ; vertical image resolution in DPI
       (pal  :: binary bytes 48) ; EGA palette for 16-color images
       (#x00 :: bytes 1) ; reserved
       (4    :: integer bytes 1) ; number of color planes
       (bpl  :: little-endian integer bytes 2) ; number of bytes of one color plane representing a single scan line
       (1    :: little-endian integer bytes 2) ; the palette contains monochrome or color information
       (640  :: little-endian integer bytes 2) ; horizontal resolution of the source system's screen
       (400  :: little-endian integer bytes 2) ; vertical resolution of the source system's screen
       (pad  :: binary bytes 54) ; reserved
       (dat  :: binary bytes len))))) ; image data

;(with-output-to-file "test.pcx" #:exists 'replace (lambda () (write-bytes (pic->pcx-bytes p))))

(provide pic->pcx-bytes)
