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
   [("--dict") b         "[decompile] dictionary base (80*, D0)"
                         (cfg:dict (string->number b 16))]
   [("--protag") p       "[decompile] protagonist name function (e.g. 0, 3, Z)"
                         (cfg:protag (string->protag p))]
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

(define (display-color c s)   (with-colors c (lambda () (display s))))
(define (displayln-color c s) (with-colors c (lambda () (displayln s))))

(define (decompile filename)
  (display-color 'b-white filename)
  (flush-output)
  (cond
   [(extension? filename ".mes")
    (let* ([mes (load-mes filename)]
           [src (pretty-format mes #:mode 'write)]
           [outname (string-append filename ".rkt")])
      (with-output-to-file outname #:exists (exists)
        (λ () (display src)))
      (displayln-color 'b-green ".rkt"))]
   [else (displayln-color 'b-yellow "?")])
  (flush-output))

(define (compile filename)
  (display-color 'b-white filename)
  (flush-output)
  (cond
   [(extension? filename ".rkt")
    (let ([mes (compile-mes filename)]
          [outname (string-append filename ".mes")])
      (with-output-to-file outname #:exists (exists)
        (λ () (write-bytes mes)))
      (displayln-color 'b-blue ".mes"))]
   [else (displayln-color 'b-yellow "?")])
  (flush-output))
 
(case (command)
 ['decompile (work decompile)]
 ['compile   (work compile)]
 ['version   (displayln (format "juice ~a by tomyun" version))]
 [else       (displayln "type `juice -h` for help")])
