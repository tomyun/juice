#lang racket/base

(require racket/cmdline)
(require racket/match)
(require racket/pretty)
(require racket/string)

(require "mes-loader.rkt")
(require "mes-builder.rkt")

(define version "v0.0.1+20210609")
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
   #:ps "<args> : filenames"
   #:args args
   args))

(define (work proc)
  (for ([f filenames])
    (with-handlers ([exn:fail? (λ (v) (displayln "!") (displayln (exn-message v)))])
      (proc f))))

(define (extension? filename ext)
  (or (string-suffix? filename (string-downcase ext))
      (string-suffix? filename (string-upcase ext))))
  
(define (decompile filename)
  (display filename)
  (cond
   [(extension? filename ".mes")
    (let* ([mes (load-mes filename)]
           [src (pretty-format mes #:mode 'write)]
           [outname (string-append filename ".rkt")])
      (with-output-to-file outname #:exists (exists)
        (λ () (display src)))
      (displayln ".rkt"))]
   [else (displayln "?")]))

(define (compile filename)
  (display filename)
  (cond
   [(extension? filename ".rkt")
    (let ([mes (compile-mes filename)]
          [outname (string-append filename ".mes")])
      (with-output-to-file outname #:exists (exists)
        (λ () (write-bytes mes)))
      (displayln ".mes"))]
   [else (displayln "?")]))
 
(case (command)
 ['decompile (work decompile)]
 ['compile   (work compile)]
 ['version   (displayln (format "juice ~a by tomyun" version))]
 [else       (displayln "type `juice -h` for help")])
