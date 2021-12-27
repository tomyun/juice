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

(require "../mes/mes-config.rkt")
(require "../mes/mes-color.rkt")
(require "../mes/mes-engine.rkt")
(require "dat-handler.rkt")

(cfg:usedict #f)

(define title "unit")
(define version "v0.0.1+20211227")
(define command (make-parameter null))
(define exists (make-parameter 'error))

(define filenames
  (command-line
   #:program title
   #:once-any
   [("--decompile" "-d")   "decompile UNIT.DAT file into rkt"
                           (command 'decompile)]
   [("--compile" "-c")     "compile rkt source into UNIT.DAT file"
                           (command 'compile)]
   [("--version" "-v")     "show version"
                           (command 'version)]
   #:once-each
   [("--force" "-f")       "force overwrite output files"
                           (exists 'replace)]
   [("--charset") c        "charset encoding (pc98*, english, europe, korean-..)"
                           (cfg:charset c)]
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

(define (decompile path)
  (save-rkt path))

(define (compile path)
  (save-dat path))

(define (format-rkt dat)
  (pretty-format dat #:mode 'write))

(define (save-rkt path)
  (define (r f) (format-rkt (load-dat f)))
  (save path ".dat" ".rkt" r display 'b-green))

(define (save-dat path)
  (define (r f) (build-dat f))
  (save path ".rkt" ".dat" r write-bytes 'b-blue))

(define (save path ext0 ext1 r w color)
  (let-values ([(b f d) (split-path path)])
    (display-color 'b-white f))
  (flush-output)
  (cond
   [(extension? path ext0)
    (let ([outname (path-add-extension path ext1 ".")])
      (with-output-to-file outname #:exists (exists)
        (λ () (w (r path))))
      (displayln-color color ext1))]
   [else (displayln-color 'b-yellow "?")])
  (flush-output))

(case (command)
 ['decompile   (work decompile)]
 ['compile     (work compile)]
 ['version     (displayln (format "~a ~a by tomyun" title version))]
 [else         (displayln (format "type `~a -h` for help" title))])
