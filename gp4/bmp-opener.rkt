#lang racket

(require bitsyntax)
(require 2htdp/image)

(define (bmp-palette-size n)
  (* 4 n))

(define (bit-string->bmp-pal b)
  (define l (bytes->list (bit-string->bytes b)))
  (define (: l)
    (flatten (match l
              [`(,b ,g ,r ,a ,n ...) `(,(make-color r g b) ,(: n))]
              [_                     '()])))
  (: l))

(define (bmp-row-size bpp w)
  (* (ceiling (/ (* bpp w) 32)) 4))
 
(define (bit-string->bmp-data b bpp w)
  (define l (bytes->list (bit-string->bytes b)))
  (define z (bmp-row-size bpp w))
  (define (unpack v i)
    (define s (- (- 8 (* (add1 i) bpp))))
    (define m (sub1 (expt 2 bpp)))
    (bitwise-and (arithmetic-shift v s) m))
  (define (split v)
    (define n (/ 8 bpp))
    (for/list ([i (in-range n)]) (unpack v i)))
  (define (scan r)
    (take (flatten (map split r)) w))
  (define (: l)
    (match l
     ['() '()]
     [_   `(,(scan (take l z)) ,@(: (drop l z)))]))
  (flatten (reverse (: l))))

(define (open-bmp-bytes b)
  (bit-string-case b
     ;; Bitmap file header (14 bytes)
   ([(= #"BM"                :: binary bytes 2)
     (file-size              :: little-endian integer bytes 4)
     (reserve1               :: little-endian integer bytes 2)
     (reserve2               :: little-endian integer bytes 2)
     (offset                 :: little-endian integer bytes 4)
     ;; DIB header (40 bytes)
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
     (palette                :: binary bytes (bmp-palette-size colors-count))
     ;; Gap
     (gap1                   :: binary bytes (- offset 14 40 (bmp-palette-size colors-count)))
     ;; Pixel Storage
     (pixels                 :: binary bytes (* (bmp-row-size bits-per-pixel width) height)) ; image-size might be zero
     ;; Gap
     (gap2                   :: binary bytes (apply - `(,(bytes-length b) 14 40 ,@(map (compose bytes-length bit-string->bytes) `(,palette ,gap1 ,pixels)))))]
    (cond
     [(not (= info-size 40))     (error (format "Unsupported DIB header size: ~a" info-size))]
     [(not (= compression 0))    (error (format "Unsupported compression method: ~a" compression))]
     [else                       (list 'BMP width height
                                            (bit-string->bmp-pal palette)
                                            (bit-string->bmp-data pixels bits-per-pixel width))]))))

(define (load-bytes path) (port->bytes (open-input-file path)))
(define (open-bmp path) (open-bmp-bytes (load-bytes path)))

(provide open-bmp-bytes
         open-bmp)
