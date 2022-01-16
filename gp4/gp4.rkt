#lang racket/base

(require racket/cmdline)
(require racket/file)
(require racket/list)
(require racket/match)
(require racket/path)
(require racket/pretty)
(require racket/string)
(require file/glob)
(require file/sha1)

(require "../mes/mes-color.rkt")

(require "gp4-loader.rkt")
(require "gp4-builder.rkt")

(require "bmp-loader.rkt")
(require "bmp-builder.rkt")

(define title "gp4")
(define version "v0.0.1+20220115")
(define command (make-parameter null))
(define exists (make-parameter 'error))

(define filenames
  (command-line
   #:program title
   #:once-any
   [("--decode" "-d")      "decode GP4 image to BMP file"
                           (command 'decode)]
   [("--encode" "-e")      "encode BMP file to GP4 image"
                           (command 'encode)]
   [("--version" "-v")     "show version"
                           (command 'version)]
   #:once-each
   [("--force" "-f")       "force overwrite output files"
                           (exists 'replace)]
   #:ps "<args> : filenames"
   #:args args
   args))

(define paths (flatten (map glob filenames)))

(define (work proc)
  (for ([p paths])
    (with-handlers ([exn:fail? (λ (v) (displayln-color 'b-red "!") (displayln (exn-message v)))])
      (proc p))))

(define (extension? path ext)
  (or (path-has-extension? path (string-downcase ext))
      (path-has-extension? path (string-upcase ext))))

(define (decode path)
  (save-bmp path))

(define (encode path)
  (save-gp4 path))

(define (save-bmp path)
  (define (r f) (pic->bmp-bytes (load-gp4 f)))
  (define (ext1 f) (format "~a.bmp" (pic->gp4-suffix (load-gp4 f)))) ;HACK: load-gp4 called twice
  (save path ".gp4" ext1 r display 'b-green))

(define (save-gp4 path)
  (define (r f) (pic->gp4-bytes (load-bmp f)))
  (save path ".bmp" ".gp4" r write-bytes 'b-blue))

(define (save path ext0 ext1 r w color)
  (let-values ([(b f d) (split-path path)])
    (display-color 'b-white f))
  (flush-output)
  (define detected
   (cond
    [(string? ext0) (extension? path ext0)]
    [else           (ext0 path)]))
  (define suffix
   (cond
    [(string? ext1) ext1]
    [else           (ext1 path)]))
  (cond
   [detected
    (let ([outname (path-add-extension path suffix ".")])
      (with-output-to-file outname #:exists (exists)
        (λ () (w (r path))))
      (displayln-color color suffix))]
   [else (displayln-color 'b-yellow "?")])
  (flush-output))

(case (command)
 ['decode      (work decode)]
 ['encode      (work encode)]
 ['version     (displayln (format "~a ~a by tomyun" title version))]
 [else         (displayln (format "type `~a -h` for help" title))])
