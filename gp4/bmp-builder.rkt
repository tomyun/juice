#lang racket/base

(require racket/list)
(require racket/match)

(require 2htdp/image)
(require bitsyntax)

(require "pic-struct.rkt")

(define (pic->bmp-file-size p)
  (+ (pic->bmp-offset p)
     (pic->bmp-image-size p)))

(define (pic->bmp-offset p)
  (+ 14
     40
     (* 4 (length (pic-pal p)))))

(define (pic->bmp-image-size p)
  (* (pic-w p) (pic-h p) (/ 4 8)))

(define (pic->bmp-pallette p)
  (define P (pic-pal p))
  (define (: c)
    `(,(color-blue  c)
      ,(color-green c)
      ,(color-red   c)
      0))
  (list->bytes (flatten (map : P))))

(define (bmp-row-size bpp w)
  (* (ceiling (/ (* bpp w) 32)) 4))

(define (pic->bmp-pixels p)
  (define l (pic-data p))
  (define w (pic-w p))
  (define bpp 4)
  (define z (bmp-row-size bpp w))
  (define (pack . V)
    (define n (length V))
    (define m (sub1 (expt 2 bpp)))
    (define r (for/list ([i (in-range n)]
                         [v V])
                 (define s (- 8 (* (add1 i) bpp)))
                 (arithmetic-shift (bitwise-and v m) s)))
    (apply + r))
  (define (merge l)
    (match l
     ['()             '()]
     [`(,a)           `(,(pack a 0))]
     [`(,a ,b ,r ...) `(,(pack a b) ,@(merge r))]))
  (define (scan l)
    (define r (merge l))
    (define f (- z (length r)))
    `(,@r ,@(make-list f 0)))
  (define (: l)
    (match l
     ['() '()]
     [_   `(,(scan (take l w)) ,@(: (drop l w)))]))
  (list->bytes (flatten (reverse (: l)))))

(define (pic->bmp-bytes p)
         ;; Bitmap file header
  (let* ([id                     #"BM"]
         [file-size              (pic->bmp-file-size p)]
         [reserve1               0]
         [reserve2               0]
         [offset                 (pic->bmp-offset p)]
         ;; DIB header
         [info-size              40]
         [width                  (pic-w p)]
         [height                 (pic-h p)]
         [planes                 1]
         [bits-per-pixel         4]
         [compression            0]
         [image-size             (pic->bmp-image-size p)]
         [horizontal-resolution  2835]
         [vertical-resolution    2835]
         [colors-count           (length (pic-pal p))]
         [important-colors-count colors-count]
         ;; Color table
         [pallette               (pic->bmp-pallette p)]
         ;; Pixel Storage
         [pixels                 (pic->bmp-pixels p)])
    (bit-string->bytes
      (bit-string
       ;; Bitmap file header
       (id                     :: binary bytes 2)
       (file-size              :: little-endian integer bytes 4)
       (reserve1               :: little-endian integer bytes 2)
       (reserve2               :: little-endian integer bytes 2)
       (offset                 :: little-endian integer bytes 4)
         ;; DIB header
       (info-size              :: little-endian integer bytes 4)
       (width                  :: little-endian integer bytes 4)
       (height                 :: little-endian integer bytes 4)
       (planes                 :: little-endian integer bytes 2)
       (bits-per-pixel         :: little-endian integer bytes 2)
       (compression            :: little-endian integer bytes 4)
       (image-size             :: little-endian integer bytes 4)
       (horizontal-resolution  :: little-endian integer bytes 4)
       (vertical-resolution    :: little-endian integer bytes 4)
       (colors-count           :: little-endian integer bytes 4)
       (important-colors-count :: little-endian integer bytes 4)
       ;; Color table
       (pallette               :: binary bytes (* 4 colors-count))
       ;; Pixel Storage
       (pixels                 :: binary bytes image-size)))))

; (with-output-to-file "test.bmp" #:exists 'replace (lambda () (write-bytes (pic->bmp-bytes p))))

(provide pic->bmp-bytes)
