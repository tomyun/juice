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

(require "mes-color.rkt")
(require "mes-config.rkt")
(require "mes-engine.rkt")

(define title "juice")
(define version "v0.0.6+20211222")
(define command (make-parameter null))
(define exists (make-parameter 'error))

(define filenames
  (command-line
   #:program title
   #:once-any
   [("--decompile" "-d")   "decompile MES bytecode into rkt source"
                           (command 'decompile)]
   [("--deduplicate" "-D") "deduplicate common procs in rkt source"
                           (command 'deduplicate)]
   [("--compile" "-c")     "compile rkt source into MES bytecode"
                           (command 'compile)]
   [("--show-preset" "-P") "show supported presets"
                           (command 'preset)]
   [("--version" "-v")     "show version"
                           (command 'version)]
   #:once-each
   [("--force" "-f")       "force overwrite output files"
                           (exists 'replace)]
   [("--preset" "-p") p    "preset for a specific game; see `--show-preset`"
                           (use-preset p)]
   [("--engine") e         "engine type (AI5*, AI1)"
                           (set-engine (string->symbol (string-upcase e)))]
   [("--charset") c        "charset encoding (pc98*, english, europe, korean-..)"
                           (cfg:charset c)]
   [("--dictbase") b       "[AI5] dictionary base (80*, D0)"
                           (cfg:dictbase (string->number b 16))]
   [("--extraop")          "[AI5] support incompatible opcodes found in some games"
                           (cfg:extraop #t)]
   [("--no-decode")        "{decompile} skip SJIS character decoding"
                           (cfg:decode #f)]
   [("--no-resolve")       "{decompile} skip cmd/sys name resolution"
                           (cfg:resolve #f)]
   [("--protag") p         "{decompile} proc/call(s) fused in text (none*, all, 0, Z, ..)"
                           (set-protag p)]
   [("--wordwrap") w       "{compile} threshold for word wrapping"
                           (cfg:wordwrap (string->number w))]
   [("--no-compress")      "{compile} skip text compression with dict"
                           (cfg:compress #f)]
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
  (save-mes path))

(define (deduplicate)
  ;; collect procs across all source files
  (define (extract l)
    (match l
      [`((define-proc ,n ,b) ,r ...) #:when (not (equal? b '(<>))) `((define-proc ,n ,b) ,@(extract r))]
      [`(,a                  ,r ...)                               `(,@(extract r))]
      [a                                                           a]))
  (define h (make-hash))
  (define (remember d) (hash-update! h d add1 0))
  (for ([p paths])
    (map remember (remove-duplicates (extract (file->value p))))
    (display ".")
    (flush-output))
  (displayln "")

  ;; find out common procs shared more than once in multiple files
  (define procs
    (for/hash ([(k v) (in-hash h)] #:when (> v 1))
      (match-define `(define-proc ,n ,b) k)
      (define c (build-mes-source k))
      (define f (format "_proc_~a_~a"
                        n
                        (bytes->hex-string (integer->integer-bytes (equal-hash-code c) 8 #t))))
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
  (for ([p paths])
    (save-patched-rkt p (patch (file->value p)))))

(define mes-style-table
  (pretty-print-extend-style-table #f
    '(define-proc if-else set-arr~ set-arr~b set-arr~c set-arr~d set-reg: set-reg:: set-reg:d set-var slot)
    '(define      if      send     send      send      send      set!     set!      set!      set!    set!)))
(define (format-rkt mes)
  (parameterize [(pretty-print-current-style-table mes-style-table)]
    (pretty-format mes #:mode 'write)))

(define (save-rkt path)
  (define (r f) (format-rkt (load-mes f)))
  (save path ".mes" ".rkt" r display 'b-green))

(define (save-proc-rkt name mes)
  (define (r f) (format-rkt mes))
  (save name "" ".rkt" r display 'b-magenta))

(define (save-patched-rkt path mes)
  (define (r f) (format-rkt mes))
  (save path ".rkt" ".rkt" r display 'b-cyan))

(define (save-mes path)
  (define (r f) (build-mes f))
  (save path ".rkt" ".mes" r write-bytes 'b-blue))

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

; (begin-for-syntax
;   (define pc98 (file->list "charset/_charset_pc98.rkt")))

(case (command)
 ['decompile   (work decompile)]
 ['deduplicate (deduplicate)]
 ['compile     (work compile)]
 ['preset      (show-preset)]
 ['version     (displayln (format "~a ~a by tomyun" title version))]
 [else         (displayln (format "type `~a -h` for help" title))])
