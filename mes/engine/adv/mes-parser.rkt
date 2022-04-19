#lang racket/base

(require racket/list)
(require racket/match)

(require bitsyntax)
(require parsack)

(require "../../mes-config.rkt")

;; parsack

(define-syntax-rule (@ p) (λ (x) (p x)))
(define-syntax-rule (:- p ...) (parser-seq p ...))
(define-syntax-rule (:~ p ...) (parser-one p ...))
(define-syntax-rule (:% p ...) (parser-compose p ...))
(define ($list t p) (>>= p (λ (x) (return (list t x)))))
(define ($cons t p) (>>= p (λ (x) (return (cons t x)))))

(define (char-between a b) (satisfy (λ (c) (char<=? a c b))))
(define (times n p)
  (cond
    [(positive? n) (:% (x <- p)
                       (xs <- (times (sub1 n) p))
                       (return (cons x xs)))]
    [(zero? n) (return '())]))
(define (optional p) (:% (<or> p (return '()))))

;; lexer-util

;; (define (lex-reg c1 c2)
;;   (match-define `(,i1 ,i2) (map char->integer `(,c1 ,c2)))
;;   (define v  (+ (* i1 #x100) i2))
;;   (define v1 (sub1 (bitwise-and v #x03FF)))
;;   (define offset (add1 (bitwise-and v1 #x0007)))
;;   (define slot   (arithmetic-shift v1 -3))
;;   (define index  (+ (* 8 slot) offset) -1)
;;   (define mode   (arithmetic-shift (bitwise-and v #x0400) -10))
;;   `(reg* ,index ,mode))

(define (lex-reg c1 c2)
  (define l (apply bytes (map char->integer `(,c1 ,c2))))
  (bit-string-case l
    ([(= 0 :: bits 4)
      (_   :: bits 1)
      (f   :: bits 1)
      (i   :: bits 7)
      (o   :: bits 3)]
     (let* ([n (+ (* 8 i) o -1)])
       `(reg* ,n '= ,f)))))

;; (define (lex-var c1 c2)
;;   (match-define `(,i1 ,i2) (map char->integer `(,c1 ,c2)))
;;   (define v (+ (* i1 #x100) i2))
;;   (define value (bitwise-and v #x000F))
;;   (define index (arithmetic-shift (bitwise-and v #x03F0) -4))
;;   (define name  (integer->char (+ (char->integer #\A) index -1)))
;;   (define mode  (arithmetic-shift (bitwise-and v #x0C00) -10))
;;   `(var* ,name ,value ,mode))

(define (integer->var i)
  `(var ,(integer->char (+ (char->integer #\A) i))))

(define (lex-var c1 c2)
  (define l (list->bytes (map char->integer `(,c1 ,c2))))
  (bit-string-case l
    ([(= 1 :: bits 4)
      (f   :: bits 2)
      (i   :: bits 6)
      (v   :: bits 4)]
     (let* ([c (integer->var i)]
            [s (match f
                 [3 ''+=]
                 [1 ''-=]
                 [_ ''=])])
       `(var* ,c ,s ,v)))))

;; (* (reg* 3 '= 0) (reg* 5 '= 1))
;; (* (& (= (~ 3) 0) (= (~ 5) 1)))
;; (* (& (~?! 3) (~? 5)))
;; (* (? (~ 3) (~! 5)))
;; (* (? (@ W 3) (@ O 4)))
;; (set-reg~ 3 0)
;; (set-reg~ 5 1)
;; (reg~= 5 1)
;; (set-var@ W 3)
;; (var@= W 3)
;; (inc-var@ W 2)
;; (var@+ W 3)
;; (dec-var@ W 1)
;; (var@- W 1)


(define (lex-var2 c1 c2 c3)
  (define l (list->bytes (map char->integer `(,c1 ,c2 ,c3))))
  (bit-string-case l
    ([(= 1 :: bits 4)
      (f   :: bits 3)
      (m   :: bits 1)
      (_   :: bits 2)
      (i   :: bits 5)
      (j1  :: bits 1)
      (_   :: bits 1)
      (j2  :: bits 7)]
     (let* ([j (bit-string->integer (bit-string (j1 :: bits 1) (j2 :: bits 7)) #t #f)]
            [c (integer->var i)]
            [v (if (= m 1) (integer->var j) j)]
            [s (match f
                 [0 ''=]
                 [1 ''+=]
                 [2 ''-=]
                 [_ ''=])])
       `(var* ,c ,s ,v)))))

(define (lex-num0 c)        `(num ,(- (char->integer c) #x23)))
(define (lex-num1 c1 c2)    `(num ,(char->integer c2)))
(define (lex-num2 c1 c2 c3)
  (match-define `(,i1 ,i2 ,i3) (map char->integer `(,c1 ,c2 ,c3)))
  `(num ,(+ (* (- i1 #x29) #x4000)
            (* i2 #x80)
            (bitwise-and i3 #x7F))))

;;TODO: merge lex-chr* functions
(define (lex-chr1  c)          `(chr-sjis2  #x82 ,(+ #x72 (char->integer c))))
(define (lex-chr2  c1 c2)      `(chr-sjis2  ,@(map char->integer `(,c1 ,c2))))
(define (lex-chr2+ c1 c2)      `(chr-sjis2+ ,@(map char->integer `(,c1 ,c2))))

(define (lex-chr-byte   c)     `(chr-byte   ,c))
(define (lex-chr-ascii  c)     `(chr-ascii  ,c))
(define (lex-chr-sjis1  c)     `(chr-sjis1  ,(char->integer c)))
(define (lex-chr-sjis2  c1 c2) `(chr-sjis2  ,@(map char->integer `(,c1 ,c2))))
(define (lex-chr-sjis2+ c1 c2) `(chr-sjis2+ ,@(map char->integer `(,c1 ,c2))))

(define cBYTE   (:% (c <- (char-between #\u00 #\uFF))
                    (return (lex-chr-byte c))))

(define cASCII  (:% (c <- (char-between #\u20 #\u7E))
                    (return (lex-chr-ascii c))))

(define cSJIS1  (:% (c <- (char-between #\uA1 #\uDF))
                    (return (lex-chr-sjis1 c))))

(define cSJIS2  (:% (c1 <- (<or> (char-between #\u80 #\u9F)
                                 (char-between #\uE0 #\uEA)))
                    (c2 <- (<or> (char-between #\u40 #\u7E)
                                 (char-between #\u80 #\uFC)))
                    (return (lex-chr-sjis2 c1 c2))))

(define cSJIS2+ (:% (c1 <- (char-between #\uEB #\uEF))
                    (c2 <- (<or> (char-between #\u40 #\u7E)
                                 (char-between #\u80 #\uFC)))
                    (return (lex-chr-sjis2+ c1 c2))))

;; lexer

(define REG*  (:% (c1 <- (char-between #\u00 #\u0F))
                  (c2 <- $anyChar)
                  (return (lex-reg c1 c2))))

(define VAR1* (:% (c1 <- (char-between #\u10 #\u1F))
                  (c2 <- $anyChar)
                  (return (lex-var c1 c2))))

(define VAR2* (:% (c1 <- (char-between #\u10 #\u1F))
                  (c2 <- $anyChar)
                  (c3 <- $anyChar)
                  (return (lex-var2 c1 c2 c3))))

(define VAR*  (:% (x? <- (getState 'extraop))
                  (if x? VAR2* VAR1*)))

(define VAR   ($list 'var (char-between #\A #\Z)))

; no general use of #\u20 (exclusively used by D0)
(define SPC (char #\u20))

(define CHRS! (:% (char #\u21)
                  (l <- (manyTill (<any> cASCII cSJIS1 cSJIS2 cSJIS2+)
                                  (<any> (char #\u00) (char #\uFF))))
                  (return `(chrs! ,@l))))

(define ARG (:% (char #\u22)
                (l <- (manyTill (<any> cASCII cSJIS1 cSJIS2 cBYTE) (char #\u22)))
                (return `(arg ,@l))))

(define NUM0 (:% (c <- (char-between #\u23 #\u27))
                 (return (lex-num0 c))))

(define NUM1 (:% (c1 <- (char #\u28))
                 (c2 <- $anyChar)
                 (return (lex-num1 c1 c2))))

(define NUM2 (:% (c1 <- (char-between #\u29 #\u2C))
                 (c2 <- $anyChar)
                 (c3 <- $anyChar)
                 (return (lex-num2 c1 c2 c3))))

(define NUM  (<or> NUM0 NUM1 NUM2))

(define CHR1  (:% (c <- (char-between #\u2D #\u7F))
                  (return (lex-chr1 c))))

(define CHR2  (:% (c1 <- (<or> (char-between #\u80 #\u9F)
                               (char-between #\uE0 #\uEA)))
                  (c2 <- (<or> (char-between #\u40 #\u7E)
                               (char-between #\u80 #\uFC)))
                  (return (lex-chr2 c1 c2))))

(define CHR2+ (:% (c1 <- (char-between #\uEB #\uEF))
                  (c2 <- (<or> (char-between #\u40 #\u7E)
                               (char-between #\u80 #\uFC)))
                  (return (lex-chr2+ c1 c2))))

(define CHR-LBEG (try (:- (char #\u81) (char #\u6F))))
(define CHR-LEND (try (:- (char #\u81) (char #\u70))))
(define CHR-WAIT (try (:- (char #\u81) (char #\u90))))
(define CHR-NOP  (try (:- (char #\u81) (char #\u97))))

(define CHR$     (<or> CHR-LBEG
                       CHR-LEND
                       CHR-WAIT
                       CHR-NOP))
(define CHR2*    (>> (notFollowedBy CHR$) (<or> CHR2 CHR2+)))

(define CHR  (<or> CHR1 CHR2*))
(define CHRS ($cons 'chrs (many1 CHR)))

(define BEG+ (char #\uA0))
(define END* (char #\uA1))
(define BEG  (char #\uA2))
(define END  (char #\uA3))
(define CNT  (char #\uA4))

(define CMD ($list 'cmd (<or> (char-between #\uA5 #\uDF))))

; unknown #\uFD

(define EOM (try (:- (char #\uFF) (char #\uFE))))
(define EOS (try (:- (char #\uFF) (char #\uFF))))

;; parser

(define reg* REG*)
(define var* VAR*)

(define reg! (:% (l <- reg*)
                 (match-let ([`(reg* ,n ,s ,f) l])
                   (let ([o (match s
                              [''= 'set-reg])]
                         [v (match f
                              [0 #f]
                              [1 #t])])
                     (return `(,o ,n ,v))))))

(define var! (:% (l <- var*)
                 (match-let ([`(var* ,c ,s ,v) l])
                   (let ([o (match s
                              [''+= 'inc-var]
                              [''-= 'dec-var]
                              [''=  'set-var])])
                     (return `(,o ,c ,v))))))

(define reg? (:% (l <- reg*)
                 (match-let ([`(reg* ,n ,s ,f) l])
                   (let ([o (match s
                              [''= '=])]
                         [v (match f
                              [0 #f]
                              [1 #t])])
                     (return `(,o ,n ,v))))))

(define var? (:% (l <- var*)
                 (match-let ([`(var* ,c ,s ,v) l])
                   (let ([o (match s
                              [''+= '>=]
                              [''-= '<=]
                              [''=  '=])])
                     (return `(,o ,c ,v))))))

(define block-begin BEG)
(define block-end   (<or> END
                          (lookAhead EOS) ;HACK: implicit END closed by the end of segment (photo/GRS25)
                          (lookAhead CHR-LEND))) ;HACK: implicit END closed by CHR-LEND (marine/000066)

(define loop-begin  CHR-LBEG)
(define loop-end    (<or> CHR-LEND
                          (lookAhead EOS))) ;HACK: dangling loop (kounai1/Y2END)

(define param  ($list 'param (<or> NUM
                                   ARG
                                   (@ block)
                                   (@ block*)
                                   (@ block/))))
(define params ($cons 'params (many param)))

(define text (<or> CHRS! CHRS))

;; operations need special handing

(define op-chr (<or> (:% (c <- CHR-WAIT) (return `(wait$)))
                     (:% (c <- CHR-NOP)  (return `(nop@)))))

;(define loop  (try ($cons 'loop  (:~ CHR-LBEG (~> (@ stmts)) CHR-LEND))))
;(define loop! (try ($cons 'loop! (:~ CHR-LBEG (~> (@ stmts)))))) ;;HACK: dangling loop (kounai1/Y2END)
;(define loops (<or> loop loop!))
(define loops  (try ($cons 'loop  (:~ loop-begin (~> (@ stmts)) loop-end))))

(define op-menu (:% (c <- (<or> (char #\uAD) (char #\uAE)))
                    (n <- (many NUM)) ;HACK: optional number args (necro/NB_02B)
                    (t <- (many text))
                    (b <- (<or> block/ block*))
                    (return `((cmd ,c) ,@n ,@t ,b))))

(define op-if-when (:% (c <- (<or> (char #\uBC) (char #\uBD)))
                       (b <- block/?)
                       (return `((cmd, c) ,b))))

(define op-branch-var (:% (c <- (char #\uB4))
                          (v <- VAR)
                          (b <- block/)
                          (return `((cmd ,c) ,v ,b))))

(define op-execute-var (:% (c <- (char #\uB6))
                           (p <- (<or> (:- ARG)
                                       (:- VAR NUM)))
                           (return `((cmd ,c) ,@p))))

(define op-CA (:% (c <- (char #\uCA))
                  (v <- VAR)
                  (n <- (many NUM)) ;HACK: not necessarily (times 3 NUM) (pia/MOVE31B)
                  (return `((cmd ,c) ,v ,@n))))

;; (define op-sound (:% (c <- (char #\uD0))
;;                      (f <- (optional (:% (f <- (many1 $alphaNum)) ;; se, fm, pm, cd
;;                                          (many SPC)
;;                                          (return (string->symbol (list->string f))))))
;;                      (p <- params)
;;                      (return `((cmd ,c) (params ,(if (empty? f) #f `',f) ,@(rest p))))))

(define op-sound (:% (c <- (char #\uD0))
                     (f <- (:% (f <- (many $alphaNum)) ;; se, fm, pm, cd
                               (s <- (many SPC))
                               (return (string->symbol (list->string `(,@f ,@s))))))
                     (p <- params)
                     (return `((cmd ,c) (params ',f ,@(rest p))))))

(define op-decrypt (:% (c <- (char #\uD5))
                       (n <- NUM)
                       (r <- (if (= (cadr n) 0)
                                 (return '())
                                 (many (<!> EOS))))
                       (return (if (empty? r)
                                   `((cmd, c) ,n)
                                   `((cmd ,c) ,n ,(parse-result <mes> (list->string (decrypt r (cadr n)))))))))

(define (decrypt l k)
  (define (flip v) (bitwise-ior (arithmetic-shift (bitwise-and v #xF0) -4)
                                (arithmetic-shift (bitwise-and v #x0F) 4)))
  (define (flop v) (bitwise-xor v k))
  (define (f    v) (flop (flip v)))
  (map integer->char (map f (map char->integer l))))
;; (bytes->hex-string (list->bytes (map char->integer db)))

;; (define op-cmd* (<or> op-chr
;;                       op-menu
;;                       op-branch-var
;;                       op-execute-var
;;                       ;op-CA ;; only for later engines
;;                       op-sound
;;                       op-decrypt)) ;; only for later engines

(define op-cmd* (:% (x? <- (getState 'extraop))
                    (<or> op-chr
                          op-menu
                          op-if-when
                          op-branch-var
                          op-execute-var
                          (if x? op-CA $err)
                          op-sound
                          (if x? op-decrypt $err))))

(define op-cmd  (:- CMD params))

(define op (<or> reg! var!
                 op-cmd*
                 op-cmd))

(define conds (:% (c <- (many (<or> reg? var?)))
                  (return `(? ,@c))))

(define stmt  (<or> loops op text))
(define stmts (many stmt))
(define block (try ($cons '<> (:~ block-begin (~> stmts) block-end))))

(define item+  (:% (c <- conds)
                   (s <- stmts)
                   (return `(+ ,c ,@s))))
;; (define item+  ($cons '+ stmts))
(define items+ (sepBy item+ CNT))
(define item   (:% (c <- conds)
                   (s <- stmts)
                   BEG+
                   (i <- items+)
                   END*
                   (return `(* ,c ,@s (<+> ,@i)))))
;; (define item   (:% (s <- stmts)
;;                    BEG+
;;                    (i <- items+)
;;                    END*
;;                    (return `(* ,@s (<+> ,@i)))))
(define items  (many1 item))
(define block* (try ($cons '<*> (:~ block-begin (~> items) block-end))))

(define branch    ($cons '/ stmts))
(define branch?   (:% (c <- conds)
                      (s <- stmts)
                      (return `(// ,c ,@s))))
(define branches  (sepBy1 branch  CNT))
(define branches? (sepBy1 branch? CNT))
(define block/    (try ($cons '</> (:~ block-begin (~> branches)  block-end))))
(define block/?   (try ($cons '</> (:~ block-begin (~> branches?) block-end))))

(define seg   (:% (c <- conds)
                  (s <- stmts)
                  (optional END) ;HACK: dangling END (marine/000012)
                  EOS
                  (return `(seg ,c ,@s))))
(define <mes> ($cons 'mes (:~ (~> (many1 seg)) EOM))) ; not always followed by $eof (PIA.INI)

(define (parser [p <mes>])
  (:% (withState (['extraop (cfg:extraop)])
                 p)))

(provide parse-result
         parse
         parser)

(provide (prefix-out p: (all-defined-out)))
