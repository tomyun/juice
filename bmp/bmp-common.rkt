#lang racket

(require racket/draw)

(define W 2048)
(define H 2048)
(define (new) (make-bitmap W H))

(define src (read-bitmap "assets/FREECG98.BMP"))
(define out (make-parameter (new)))
(define buf (make-bytes (* 16 16 4)))

(define (save outname)
  (define dst (out))
  (send dst save-file outname 'bmp)
  (system (format "convert -monochrome ~a bmp3:~a" outname outname)))

(provide W
         H
         new
         src
         out
         buf
         save)
