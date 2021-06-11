#lang racket/base

(require racket/cmdline)
(require racket/match)
(require racket/pretty)

(require "mes-loader.rkt")
(require "mes-builder.rkt")

(define version "v0.0.1+20210609")
(define command (make-parameter null))

(define filenames
  (command-line
   #:program "juice"
   #:once-any
   [("-d" "--decompile") "decompile MES bytecode into rkt source"
                         (command 'decompile)]
   [("-c" "--compile")   "compile rkt source into MES bytecode"
                         (command 'compile)]
   [("-v" "--version")   "show version"
                         (command 'version)]
   #:ps "<args> : filenames"
   #:args args
   args))

(define (work proc)
  (for ([f filenames])
    (with-handlers ([exn:fail? (λ (v) (newline) (displayln (exn-message v)))])
      (proc f))))

(define (decompile filename)
  (display filename)
  (flush-output)
  (define mes (load-mes filename))
  (define src (pretty-format mes #:mode 'write))
  (define outname (string-append filename ".rkt"))
  (define out (open-output-file outname))
  (display src out)
  (close-output-port out)
  (displayln ".rkt"))

(define (compile filename)
  (display filename)
  (flush-output)
  (define mes (compile-mes filename))
  (define outname (string-append filename ".mes"))
  (define out (open-output-file outname #:exists 'replace))
  (write-bytes mes out)
  (close-output-port out)
  (displayln ".mes"))
 
(case (command)
 ['decompile (work decompile)]
 ['compile   (work compile)]
 ['version   (displayln (format "juice ~a by tomyun" version))]
 [else       (displayln "type `juice -h` for help")])
