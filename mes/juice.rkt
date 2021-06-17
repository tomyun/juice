#lang racket/base

(require racket/cmdline)
(require racket/match)
(require racket/pretty)
(require racket/string)

(require ansi-color)

(require "mes-config.rkt")
(require "mes-loader.rkt")
(require "mes-builder.rkt")

(define version "v0.0.4-DEV+20210614")
(define command (make-parameter null))
(define exists (make-parameter 'error))

(define filenames
  (command-line
   #:program "juice"
   #:once-any
   [("--decompile""-d")  "decompile MES bytecode into rkt source"
                         (command 'decompile)]
   [("--compile" "-c")   "compile rkt source into MES bytecode"
                         (command 'compile)]
   [("--version" "-v")   "show version"
                         (command 'version)]
   #:once-each
   [("--force" "-f")     "force overwriting output files"
                         (exists 'replace)]
   [("--dict") b         "dictionary base (80*, D0)"
                         (cfg:dict (string->number b 16))]
   [("--no-decode")      "[decompile] skip SJIS character decoding"
                         (cfg:decode #f)]
   [("--no-resolve")     "[decompile] skip cmd/sys name resolution"
                         (cfg:resolve #f)]
   [("--no-compress")    "[compile] skip text compression with dict"
                         (cfg:compress #f)]
   #:ps "<args> : filenames"
   #:args args
   args))

(define (work proc)
  (for ([f filenames])
    (with-handlers ([exn:fail? (λ (v) (displayln-color 'b-red "!") (displayln (exn-message v)))])
      (proc f))))

(define (extension? filename ext)
  (or (string-suffix? filename (string-downcase ext))
      (string-suffix? filename (string-upcase ext))))

(define (display-color   c s) (with-colors c (lambda () (display   s))))
(define (displayln-color c s) (with-colors c (lambda () (displayln s))))

(define (decompile filename)
  (save-rkt filename))

(define (compile filename)
  (save-mes filename))

(define (save-rkt filename)
  (define (r f) (pretty-format (load-mes f) #:mode 'write))
  (save filename ".mes" ".rkt" r display 'b-green))

(define (save-mes filename)
  (define (r f) (compile-mes f))
  (save filename ".rkt" ".mes" r write-bytes 'b-blue))

(define (save filename ext0 ext1 r w color)
  (display-color 'b-white filename)
  (flush-output)
  (cond
   [(extension? filename ext0)
    (let ([outname (string-append filename ext1)])
      (with-output-to-file outname #:exists (exists)
        (λ () (w (r filename))))
      (displayln-color color ext1))]
   [else (displayln-color 'b-yellow "?")])
  (flush-output))

(case (command)
 ['decompile   (work decompile)]
 ['compile     (work compile)]
 ['version     (displayln (format "juice ~a by tomyun" version))]
 [else         (displayln "type `juice -h` for help")])
