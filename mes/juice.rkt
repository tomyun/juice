#lang racket/base

(require racket/cmdline)
(require racket/match)
(require racket/pretty)

(require "mes-loader.rkt")
(require "mes-builder.rkt")

(define version "v0.0.1+20210609")
(define command (make-parameter null))

(define args
  (command-line
   #:program "juice"
   #:once-any
   [("-d" "--decompile") "decompile MES bytecode into rkt source"
                         (command 'decompile)]
   [("-c" "--compile")   "compile rkt source into MES bytecode"
                         (command 'compile)]
   [("-v" "--version")   "show version"
                         (command 'version)]
   #:ps "<args> : filename"
   #:args args
   args))

(define filename
 (match args
  [`(,f) f]
  [_     #f]))

(define (decompile)
  (display filename)
  (flush-output)
  (define mes (load-mes filename))
  (define src (pretty-format mes #:mode 'write))
  (define outname (string-append filename ".rkt"))
  (define out (open-output-file outname))
  (display src out)
  (close-output-port out)
  (displayln ".rkt"))

(define (compile)
  (display filename)
  (flush-output)
  (define mes (compile-mes filename))
  (define outname (string-append filename ".mes"))
  (define out (open-output-file outname #:exists 'replace))
  (write-bytes mes out)
  (close-output-port out)
  (displayln ".mes"))
 
(case (command)
 ['decompile (decompile)]
 ['compile   (compile)]
 ['version   (displayln (format "juice ~a by tomyun" version))]
 [else       (displayln "type `juice -h` for help")])
