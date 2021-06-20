#lang racket

(require racket/draw)
(require file/sha1)

(define W 2048)
(define H 2048)

(define src (read-bitmap "assets/FREECG98.BMP"))
(define out (make-bitmap W H))
(define buf (make-bytes (* 16 16 4)))

(define (blit w h X Y)  
  (for* ([x X]
         [y Y])
    (send src get-argb-pixels x y w h buf)
    (send out set-argb-pixels x y w h buf)))

(define (init)
  ;; copy half-width glyphs in the first line
  (let ([w  8]
        [h 16])
    (blit w
          h
          (range 0 W w)
          (range 0 h h)))

  ;; copy full-width glyphs in the remaining lines
  (let ([w 16]
        [h 16])
    (blit w
          h
          (range 0 W w)
          (range h H h))))

(init)

;; copy extra!

(define (copy k0 t0 k1 t1)
  (define w 16)
  (define h 16)
  (define (x k) (* k w))
  (define (y t) (* (+ t 32) h))
  (send src get-argb-pixels (x k0) (y t0) w h buf)
  (send out set-argb-pixels (x k1) (y t1) w h buf))

(define (nk k t [i 0]) (+ k (quotient (+ (sub1 t) i) 94)))
(define (nt t [i 0])   (add1 (remainder (+ (sub1 t) i) 94)))

(define (copy* k0 t0 k1 t1 n)
  (for ([i (range n)])
    (copy (nk k0 t0 i)
          (nt t0 i)
          (nk k1 t1 i)
          (nt t1 i))))

;(copy 9 1 85 1)
;(copy* 9 1 85 1 94)

;; fallback

(define 4x4-image (read-bitmap "assets/4x4-charset.png"))
(define 4x4-chars "abcdefghijklmnopqrstuvwxyz!?.()'\",:<>1234567890#■□█ ")
(define 4x4-dict  (make-hash (map cons (string->list 4x4-chars) (range (string-length 4x4-chars)))))

(define (blit-fallback c x y)
  (define i (dict-ref 4x4-dict c))
  (send 4x4-image get-argb-pixels (* i 4) 0 4 4 buf)
  (send out set-argb-pixels x y 4 4 buf))

(define (fallback k t)
  (define x (* 16 k))
  (define y (* 16 (+ t 32)))
  (for* ([dx (range 4)]
         [dy (range 4)])
    (blit-fallback #\  (+ x (* 4 dx)) (+ y (* 4 dy))))
  (define (f n) (~r n #:min-width 2 #:pad-string "0"))
  (define h (format "~a~a" (f k) (f t)))
  (blit-fallback (string-ref h 0) (+ x  0) (+ y  0))
  (blit-fallback (string-ref h 1) (+ x  4) (+ y  0))
  (blit-fallback (string-ref h 2) (+ x  8) (+ y  4))
  (blit-fallback (string-ref h 3) (+ x 12) (+ y  4))
  (define j (sjis->integer (sjis (nk k t) (nt t))))
  (define l (bytes->hex-string (integer->integer-bytes j 2 #f #t)))
  (blit-fallback (string-ref l 0) (+ x  0) (+ y  8))
  (blit-fallback (string-ref l 1) (+ x  4) (+ y  8))
  (blit-fallback (string-ref l 2) (+ x  8) (+ y 12))
  (blit-fallback (string-ref l 3) (+ x 12) (+ y 12)))

(define (fallback* k t n)
  (for ([i (range n)])
    (fallback (nk k t i) (nt t i))))
 
;(fallback* 15 1 (* 80 94))

;; patch!
(define ref (read-bitmap "assets/unifont-13.0.06.bmp"))

(define (blit-patch u1 u2 i j)
  (define w 16)
  (define h 16)
  (let ([x (* (+ u2 2) w)]
        [y (* (+ u1 4) h)])
    (send ref get-argb-pixels x y w h buf))
  (let ([x (* i w)]
        [y (* j h)])
    (send out set-argb-pixels x y w h buf)))

(define (patch c k t)
  (displayln (format "~a ~a ~a" c k t))
  (define u (char->integer c))
  (define u1 (arithmetic-shift (bitwise-and u #xFF00) -8))
  (define u2 (bitwise-and u #x00FF))
  (define i k)
  (define j (+ t 32))
  (blit-patch u1 u2 i j))

(define (patch* l k t)
  (define n (length l))
  (for ([c l]
        [i (range n)])
    (patch c
           (+ k (quotient (+ (sub1 t) i) 94))
           (add1 (remainder (+ (sub1 t) i) 94)))))

;(blit-patch #x00 #xc0 9 55)
;(patch #\u00c0 9 22)

(define ascii
 '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
   #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
   #\: #\; #\< #\= #\> #\? #\@
   #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
   #\[ #\\ #\] #\^ #\_ #\`
   #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
   #\{ #\| #\} #\~))
(patch* ascii 48 1)

;(include "patch-table.rkt")
;(include "patch-table-full.rkt")

(define diac
 '(#\Ǎ #\ǎ #\ǐ #\Ḿ #\ḿ #\Ǹ #\ǹ #\Ǒ #\ǒ #\ǔ #\ǖ #\ǘ #\ǚ #\ǜ
   #\¿
   #\À #\Á #\Â #\Ã #\Ä #\Å #\Æ #\Ç #\È #\É #\Ê #\Ë #\Ì #\Í #\Î #\Ï #\Ð #\Ñ #\Ò #\Ó #\Ô #\Õ #\Ö #\Ø #\Ù #\Ú #\Û #\Ü #\Ý
   #\Þ #\ß #\à #\á #\â #\ã #\ä #\å #\æ #\ç #\è #\é #\ê #\ë #\ì #\í #\î #\ï #\ð #\ñ #\ò #\ó #\ô #\õ #\ö #\ø #\ù #\ú #\û #\ü #\ý #\þ #\ÿ
   #\Ā #\Ī #\Ū #\Ē #\Ō #\ā #\ī #\ū #\ē #\ō
   #\Ą #\Ł #\Ľ #\Ś #\Š #\Ş #\Ť #\Ź #\Ž #\Ż
   #\ą #\ł #\ľ #\ś #\š #\ş #\ť #\ź #\ž #\ż
   #\Ŕ #\Ă #\Ĺ #\Ć #\Č #\Ę #\Ě #\Ď #\Ń #\Ň #\Ő #\Ř #\Ů #\Ű #\Ţ
   #\ŕ #\ă #\ĺ #\ć #\č #\ę #\ě #\ď #\đ #\ń #\ň #\ő #\ř #\ů #\ű #\ţ
   #\Ĉ #\Ĝ #\Ĥ #\Ĵ #\Ŝ #\Ŭ
   #\ĉ #\ĝ #\ĥ #\ĵ #\ŝ #\ŭ
   #\ɱ #\ʋ #\ɾ #\ʃ #\ʒ #\ɬ #\ɮ #\ɹ #\ʈ #\ɖ #\ɳ #\ɽ #\ʂ #\ʐ #\ɻ #\ɭ #\ɟ #\ɲ #\ʝ #\ʎ #\ɡ #\ŋ #\ɰ #\ʁ #\ħ #\ʕ #\ʔ #\ɦ #\ʘ #\ǂ #\ɓ #\ɗ #\ʄ #\ɠ #\Ɠ #\œ #\Œ #\ɨ #\ʉ #\ɘ #\ɵ #\ə #\ɜ #\ɞ #\ɐ #\ɯ #\ʊ #\ɤ #\ʌ #\ɔ #\ɑ #\ɒ #\ʍ #\ɥ #\ʢ #\ʡ #\ɕ #\ʑ #\ɺ #\ɧ #\ɚ
   #\æ #\ǽ #\ὰ #\ά #\ɔ #\ɔ #\ʌ #\ʌ #\ə #\ə #\ɚ #\ɚ #\ὲ #\έ))
(patch* diac 49 1)

(define (sjis k t)
  (define s1 (cond [(<=  1 k 62) (floor (/ (+ k 257) 2))]
                   [(<= 63 k 94) (floor (/ (+ k 385) 2))]))
  (define s2 (cond [(even? k)    (+ t 158)]
                   [(<=  1 t 63) (+ t 63)]
                   [(<= 64 t 94) (+ t 64)]))
  `(,s1 ,s2))

(define (sjis->integer l)
  (match-define `(,c1 ,c2) l)
  (+ (arithmetic-shift c1 8) c2))

(define (sjis-tbl l k t)
  (define n (length l))
  (for/list ([i (range n)])
    (sjis (nk k t i) (nt t i))))

;(define diac-tbl (sjis-tbl diac 86 1))
;(displayln diac-tbl)

;; save
(define outname "font.bmp")
(send out save-file outname 'bmp)
(system (format "convert -monochrome ~a bmp3:~a" outname outname))
