#lang racket/base

(require racket/cmdline)
(require racket/file)
(require racket/match)
(require racket/pretty)
(require racket/string)
(require file/sha1)

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
   [("--decompile" "-d")   "decompile MES bytecode into rkt source"
                           (command 'decompile)]
   [("--deduplicate" "-D") "deduplicate common procs in rkt source"
                           (command 'deduplicate)]
   [("--compile" "-c")     "compile rkt source into MES bytecode"
                           (command 'compile)]
   [("--version" "-v")     "show version"
                           (command 'version)]
   #:once-each
   [("--force" "-f")       "force overwriting output files"
                           (exists 'replace)]
   [("--charset") c        "specify charset encoding (pc98*, europe)"
                           (cfg:charset c)]
   [("--dictbase") b       "dictionary base (80*, D0)"
                           (cfg:dictbase (string->number b 16))]
   [("--extraop")          "support incompatible opcodes found in later games"
                           (cfg:extraop #t)]
   [("--no-decode")        "[decompile] skip SJIS character decoding"
                           (cfg:decode #f)]
   [("--no-resolve")       "[decompile] skip cmd/sys name resolution"
                           (cfg:resolve #f)]
   [("--no-protag")        "[decompile] skip text fusion with protag proc/call"
                           (cfg:protag #f)]
   [("--no-compress")      "[compile] skip text compression with dict"
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

(define (deduplicate)
  ;; collect procs across all source files
  (define (extract l)
    (match l
      [`((define-proc ,n ,b) ,r ...) #:when (not (equal? b '(<>))) `((define-proc ,n ,b) ,@(extract r))]
      [`(,a                  ,r ...)                               `(,@(extract r))]
      [a                                                           a]))
  (define h (make-hash))
  (define (remember d) (hash-update! h d add1 0))
  (for ([f filenames])
    (map remember (extract (file->value f)))
    (display ".")
    (flush-output))
  (displayln "")

  ;; find out common procs shared more than once
  (define procs
    (for/hash ([(k v) (in-hash h)] #:when (> v 1))
      (match-define `(define-proc ,n ,b) k)
      (define f (format "_proc_~a_~a"
                        n
                        (bytes->hex-string (integer->integer-bytes (equal-hash-code k) 8 #t))))
      (save-proc-rkt f k)
      (values k (format "~a.rkt" f))))

  ;; replace shared proc uses in the source files
  (define (p n b) `(define-proc ,n ,b))
  (define (patch l)
    (match l
      [`((define-proc ,n ,b) ,r ...) #:when (hash-has-key? procs (p n b))
       `((include ,(hash-ref procs (p n b))) ,@(patch r))]
      [`(,a                  ,r ...)
       `(,(patch a) ,@(patch r))]
      [a a]))
  (for ([f filenames])
    (save-patched-rkt f (patch (file->value f)))))

(define (save-rkt filename)
  (define (r f) (pretty-format (load-mes f) #:mode 'write))
  (save filename ".mes" ".rkt" r display 'b-green))

(define (save-proc-rkt name mes)
  (define (r f) (pretty-format mes #:mode 'write))
  (save name "" ".rkt" r display 'b-magenta))

(define (save-patched-rkt filename mes)
  (define (r f) (pretty-format mes #:mode 'write))
  (save filename ".rkt" ".rkt" r display 'b-cyan))

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
 ['deduplicate (deduplicate)]
 ['compile     (work compile)]
 ['version     (displayln (format "juice ~a by tomyun" version))]
 [else         (displayln "type `juice -h` for help")])
