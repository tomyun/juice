#lang racket/base

(require racket/format)
(require racket/match)
(require racket/set)
(require racket/string)

(require "mes-color.rkt")

;; config

(define cfg:engine    (make-parameter 'AI5))
(define cfg:charset   (make-parameter "pc98"))
(define cfg:char-space   (make-parameter #\u3000))
;;TODO: make default char-* to be #f (need to be set through charset update)
(define cfg:char-newline  (make-parameter #\％))
(define cfg:char-continue (make-parameter #\＠))
(define cfg:char-break    (make-parameter #\＃))
(define cfg:fontwidth (make-parameter 2))
(define cfg:dictbase  (make-parameter #x80))
(define cfg:extraop   (make-parameter #f))
(define cfg:decode    (make-parameter #t))
(define cfg:resolve   (make-parameter #t))
(define cfg:protag    (make-parameter #f))
(define cfg:wordwrap  (make-parameter #f))
(define cfg:compress  (make-parameter #t))

(provide cfg:engine
         cfg:charset
         cfg:char-space
         cfg:char-newline
         cfg:char-continue
         cfg:char-break
         cfg:fontwidth
         cfg:dictbase
         cfg:extraop
         cfg:decode
         cfg:resolve
         cfg:protag
         cfg:wordwrap
         cfg:compress)

;; preset
(define (show-preset)
  (define P
    '((angel  "Angel Hearts")
      (aishi  "Ai Shimai")
      (cre    "Crescent")
      (coc    "Curse of Castle")
      (deja   "De-Ja")
      (deja2  "De-Ja 2")
      (nanpa  "Doukyuusei")
      (nanpa2 "Doukyuusei 2")
      (dk     "Dragon Knight")
      (dk2    "Dragon Knight 2")
      (dk3    "Dragon Knight 3")
      (dk4    "Dragon Knight 4")
      (elle   "ELLE")
      (foxy   "Foxy")
      (foxy2  "Foxy 2")
      (isaku  "Isaku")
      (jack   "Jack")
      (jan    "Jan Jaka Jan")
      (kakyu  "Kakyuusei")
      (kawa   "Kawarazaki-ke no Ichizoku")
      (yuno   "Kono Yo no Hate de Koi o Utau Shoujo YU-NO")
      (metal  "Metal Eye")
      (metal2 "Metal Eye 2")
      (mobius "Mobius Roid")
      (nono   "Nonomura Byouin no Hitobito")
      (pinky  "Pinky Ponky 1/2/3")
      (pre    "Premium")
      (pre2   "Premium 2")
      (raygun "RAY-GUN")
      (reira  "Reira Slave Doll")
      (syan   "Shangrlia")
      (syan2  "Shangrlia 2")
      (ten    "Tenshin Ranma")
      (shima  "Ushinawareta Rakuen")
      (ww     "Words Worth")))
  (for ([p P])
    (match-define `(,g ,d) p)
    (display-color 'b-white (~a g #:width 6))
    (displayln (format " : ~a" d))))

(define (use-preset p)
  (match (string-downcase p)
    ;["angel"   (cfg:engine 'AI1)]
    ["aishi"   (void)]
    ["cre"     (void)]
    ["coc"     (void)]
    ["deja"    (cfg:engine 'AI1)
               (cfg:protag '(Y))]
    ["deja2"   (void)]
    ["nanpa"   (cfg:protag '(0))]
    ["nanpa2"  (cfg:protag '(0))]
    ["dk"      (cfg:engine 'AI1)
               (cfg:protag '((+ (~ @ 36) (* 120 2)) (+ M (* (~ J 0) 50))))]
    ["dk2"     (cfg:engine 'AI1)
               (cfg:protag '(G))]
    ["dk3"     (cfg:protag '(26))]
    ["dk4"     (void)]
    ["elle"    (cfg:protag '(Z))]
    ;["foxy"    (void)]
    ["foxy2"   (void)]
    ["isaku"   (cfg:dictbase #xD0)
               (cfg:extraop #t)]
    ["jack"    (cfg:dictbase #xD0)]
    ["jan"     (cfg:protag '(51))]
    ["kakyu"   (cfg:dictbase #xD0)
               (cfg:protag '(3))]
    ["kawa"    (void)]
    ["yuno"    (cfg:dictbase #xD0)
               (cfg:extraop #t)
               (cfg:protag '(0))]
    ["metal"   (void)]
    ["metal2"  (void)]
    ["mobius"  (cfg:dictbase #xD0)]
    ["nono"    (void)]
    ["pinky"   (cfg:engine 'AI1)
               (cfg:protag '(N))]
    ["pre"     (void)]
    ;["pre2"    (void)]
    ["raygun"  (cfg:engine 'AI1)]
    ["reira"   (void)]
    ["syan"    (void)]
    ["syan2"   (void)]
    ["ten"     (void)]
    ["shima"   (void)]
    ["ww"      (cfg:protag '(0))]))

;; engine

(define (set-engine e)
  (cfg:engine
    (match e
      ['AI2    'AI1]
      ['AI4    'AI5]
      ['AI5X   (cfg:dictbase #xD0)
               (cfg:extraop  #t)
               'AI5]
      [e       e])))

;; protag

(define (set-protag p)
  (define (split)
    (define l (map string-trim (string-split p ",")))
    (define (: s)
      (match s
        [(regexp #rx"[0-9]+") (string->number s)]
        [(regexp #rx"[A-Z]")  (string->symbol s)]
        [s                    (error (format "unsupported protag proc/call: ~a" s))]))
    (map : l))
  (cfg:protag
    (match p
      ["all"  #t]
      ["none" #f]
      [s      (split)])))

(define (protag? x)
  (define p (cfg:protag))
  (match p
    [#t #t]
    [#f #f]
    [p  (set-member? p x)]))

(provide show-preset
         use-preset
         set-engine
         set-protag
         protag?)
