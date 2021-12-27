#lang racket/base

(require racket/file)
(require racket/list)
(require racket/match)
(require racket/port)

(require file/sha1)

(require bitsyntax)

(require "../mes/mes-config.rkt")
(require "../mes/mes-charset.rkt")
(require "../mes/engine/ai5/mes-loader.rkt")
(require "../mes/engine/ai5/mes-builder.rkt")

(define (load-dat path)
  (define b (load-bytes path))
  `(units
    (meta (charset ,(cfg:charset)))
    ,@(for/list ([i (in-range 200)]) (unit-bytes->list (subbytes b (* i 70) (* (add1 i) 70))))))

(define (fetch-charset l)
  (define (: x)
    (match x
     [`(units ,a ...) (map : a)]
     [`(meta ,a ...)  (map : a)]
     [`(charset ,v)   x]
     [_               '()]))
  (match (flatten (: l))
   [`(charset ,v) v]
   [`()           (cfg:charset)]))

(define (build-dat path)
  ;(display-to-file (pretty-format (car d) #:mode 'write) #:exists 'replace "test.rkt")
  (define src (file->value path))
  (cfg:charset (fetch-charset src))
  (define (: x)
    (match x
     [`(units ,a ...) (map : a)]
     [`(meta ,a ...)  '()]
     [`(unit ,a ...)  (unit-list->bytes x)]))
  (foldr bytes-append #"" (flatten (: src))))

(define (load-bytes path) (port->bytes (open-input-file path) #:close? #t))

(define (unit-bytes->list b)
  (bit-string-case b
   ([(troops       :: big-endian integer bytes 1)
     (index        :: big-endian integer bytes 1)
     (map          :: big-endian integer bytes 1)
     (name         :: binary bytes 16)
     (= 0          :: bytes 1)
     (class        :: big-endian integer bytes 1)
     (xp           :: big-endian integer bytes 1)
     (order        :: big-endian integer bytes 1)
     (x            :: big-endian integer bytes 1)
     (y            :: big-endian integer bytes 1)
     (flag         :: big-endian integer bits 4)
     (troop-type   :: big-endian integer bits 2)
     (attack-type  :: big-endian integer bits 2)
     (chunk1       :: binary bytes 16)
     (move         :: big-endian integer bytes 1)
     (attack       :: big-endian integer bytes 1)
     (defence      :: big-endian integer bytes 1)
     (range        :: big-endian integer bytes 1)
     (chunk2       :: binary bytes 16)
     (unknown1     :: big-endian integer bytes 1)
     (unknown2     :: big-endian integer bytes 1)
     (pad1         :: big-endian integer bytes 2)
     (unknown3     :: big-endian integer bytes 1)
     (pad2         :: big-endian integer bytes 2)
     (unknown4     :: big-endian integer bytes 1)]
    `(unit
      #:troops      ,troops
      #:index       ,index
      #:map         ,map
      #:name        ,(unit-name->mes name)
      #:class       ,(unit-class->symbol class)
      #:xp          ,xp
      #:order       ,(unit-order->symbol order)
      #:x           ,x
      #:y           ,y
      #:flag        ,flag
      #:troop-type  ,(unit-troop-type->symbol troop-type)
      #:attack-type ,(unit-attack-type->symbol attack-type)
      #:chunk1      ,(bytes->list (bit-string->bytes chunk1))
      #:move        ,move
      #:attack      ,attack
      #:defence     ,defence
      #:range       ,range
      #:chunk2      ,(bytes->list (bit-string->bytes chunk2))
      #:unknown1    ,unknown1
      #:unknown2    ,unknown2
      #:pad1        ,pad1
      #:unknown3    ,unknown3
      #:pad2        ,pad2
      #:unknown4    ,unknown4))))

(define (unit-name->mes bs)
  (load-mes-snippet* (bytes->hex-string (bit-string->bytes bs))))

(define (unit-mes->name m)
  (define b (build-mes-source m))
  (define n (bytes-length b))
  (if (<= n 16)
    (bytes-append b (make-bytes (- 16 n)))
    (error (format "oversized name ~a (~a > 16)" m n))))

(define (unit-class->symbol v)
  (match v
   [0 'swordsmen]
   [1 'axemen]
   [2 'cavalry]
   [3 'mages]
   [4 'ninjas]
   [5 'dragon-knights-ground]
   [6 'dragon-knights-air]
   [7 'agents]
   [8 'archers]
   [9 'cannoneers]))

(define (unit-symbol->class v)
  (match v
   ['swordsmen             0]
   ['axemen                1]
   ['cavalry               2]
   ['mages                 3]
   ['ninjas                4]
   ['dragon-knights-ground 5]
   ['dragon-knights-air    6]
   ['agents                7]
   ['archers               8]
   ['cannoneers            9]))

(define (unit-order->symbol v)
  (match v
   [0   'pursue-nearby]
   [1   'pursue-weak]
   [2   'occupy-base]
   [3   'restore]
   [4   'stay]
   [5   'escape]
   [6   'ambush]
   [7   'custom1]
   [8   'custom2]
   [9   'custom3]
   [255 'user]))

(define (unit-symbol->order v)
  (match v
   ['pursue-nearby 0]
   ['pursue-weak   1]
   ['occupy-base   2]
   ['restore       3]
   ['stay          4]
   ['escape        5]
   ['ambush        6]
   ['custom1       7]
   ['custom2       8]
   ['custom3       9]
   ['user          255]))

(define (unit-troop-type->symbol v)
  (match v
   [1 'ground]
   [2 'air]))

(define (unit-symbol->troop-type v)
  (match v
   ['ground 1]
   ['air    2]))

(define (unit-attack-type->symbol v)
  (match v
   [0 'none]
   [1 'ground]
   [2 'air]
   [3 'combo]))

(define (unit-symbol->attack-type v)
  (match v
   ['none   0]
   ['ground 1]
   ['air    2]
   ['combo  3]))

(define (unit-list->bytes l)
  (match-define
    `(unit
      #:troops      ,troops
      #:index       ,index
      #:map         ,map
      #:name        ,name
      #:class       ,class
      #:xp          ,xp
      #:order       ,order
      #:x           ,x
      #:y           ,y
      #:flag        ,flag
      #:troop-type  ,troop-type
      #:attack-type ,attack-type
      #:chunk1      ,chunk1
      #:move        ,move
      #:attack      ,attack
      #:defence     ,defence
      #:range       ,range
      #:chunk2      ,chunk2
      #:unknown1    ,unknown1
      #:unknown2    ,unknown2
      #:pad1        ,pad1
      #:unknown3    ,unknown3
      #:pad2        ,pad2
      #:unknown4    ,unknown4)
    l)
  (let ([name (unit-mes->name name)]
        [class (unit-symbol->class class)]
        [order (unit-symbol->order order)]
        [troop-type (unit-symbol->troop-type troop-type)]
        [attack-type (unit-symbol->attack-type attack-type)]
        [chunk1 (list->bytes chunk1)]
        [chunk2 (list->bytes chunk2)])
    (bit-string->bytes
      (bit-string
       (troops      :: big-endian integer bytes 1)
       (index       :: big-endian integer bytes 1)
       (map         :: big-endian integer bytes 1)
       (name        :: binary bytes 16)
       (0           :: bytes 1)
       (class       :: big-endian integer bytes 1)
       (xp          :: big-endian integer bytes 1)
       (order       :: big-endian integer bytes 1)
       (x           :: big-endian integer bytes 1)
       (y           :: big-endian integer bytes 1)
       (flag        :: big-endian integer bits 4)
       (troop-type  :: big-endian integer bits 2)
       (attack-type :: big-endian integer bits 2)
       (chunk1      :: binary bytes 16)
       (move        :: big-endian integer bytes 1)
       (attack      :: big-endian integer bytes 1)
       (defence     :: big-endian integer bytes 1)
       (range       :: big-endian integer bytes 1)
       (chunk2      :: binary bytes 16)
       (unknown1    :: big-endian integer bytes 1)
       (unknown2    :: big-endian integer bytes 1)
       (pad1        :: big-endian integer bytes 2)
       (unknown3    :: big-endian integer bytes 1)
       (pad2        :: big-endian integer bytes 2)
       (unknown4    :: big-endian integer bytes 1)))))

(provide load-dat
         build-dat)
