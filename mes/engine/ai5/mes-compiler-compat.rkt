(define (mes:set-reg   . l) (apply mes:set-reg:  l))
(define (mes:set-reg*  . l) (apply mes:set-reg:: l))
(define (mes:set-arr   . l) (apply mes:set-arr~  l))
(define (mes:set-arr.b . l) (apply mes:set-arr~b l))

(define mes:print-number mes:number)

(define (mes:arr   a b) (mes:~  a b))
(define (mes:arr.b a b) (mes:~b a b))
(define (mes:reg   a)   (mes::  a))
(define (mes:& . l)     (apply mes:&& l))
(define (mes:! . l)     (apply mes:// l))
(define (mes:rnd   a)   (mes:?  a))

(define (mes:begin  . l) (apply mes:<> l))
(define (mes:begin* . l) (apply mes:<*> l))
(define (mes:<>*    . l) (apply mes:<*> l))

(define (mes:load-file  . l) (apply mes:load l))
(define (mes:load-image . l) (apply mes:image l))
