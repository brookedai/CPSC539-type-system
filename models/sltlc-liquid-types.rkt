#lang racket

(require rosette
         racket/struct)
(provide (all-defined-out))


#|
SLTLC LIQUID TYPE PREDICATES

p, q ::= var                               
         #t, #f                            boolean constants
         (p-ineqop iop expr expr)          inequalities
         (p-logicop lop p p)               logical operations
         (p-not p)
         (p-if then else)
         
expr       ::= (arithop aop expr expr)     arithmetic expression
               n
               var       
var        ::= x, y, z, ...                variables
n          ::= 0, -1, 1, ...               integer constants
iop        ::= <, <=, =, >=, >, !=         inequality operators
lop        ::= &&, ||                      logic operators
aop        ::= +, -                        arithmetic operators

|#
(struct predicate () #:transparent)
(struct ineqop predicate (iop e1 e2)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:ineqop)
        (lambda (obj) (list (ineqop-iop obj) (ineqop-e1 obj) (ineqop-e2 obj)))))])
(struct logicop predicate (lop p1 p2)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:logicop)
        (lambda (obj) (list (logicop-lop obj) (logicop-p1 obj) (logicop-p2 obj)))))])
(struct arithop (aop e1 e2)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:arithop)
        (lambda (obj) (list (arithop-aop obj) (arithop-e1 obj) (arithop-e2 obj)))))])
(struct not (p)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:not)
        (lambda (obj) (list (not-p obj)))))])
(struct if (cond then else)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:if)
        (lambda (obj) (list (if-cond obj) (if-then obj) (if-else obj)))))])
(struct value-var () 
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:value-var)
        (lambda (obj) (list))))]) ; represents v, the special value variable
(struct placeholder-var () 
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:placeholder-var)
        (lambda (obj) (list))))]) ; represents the star placeholder variable

;; Templates
; (define (fn-for-expr p)
;   (cond [(arithop? p) (... (arithop-aop p) 
;                              (fn-for-expr (arithop-e1 p)) 
;                              (fn-for-expr (arithop-e2 p)))]
;         [(integer? p) ... p]
;         [(symbol? p) ... p]
;         [else (error 'fn-for-expr "invalid expr ~a" p)])
  

; (define (fn-for-predicate p)
;   (cond [(var? p) ... (var-name p)]
;         [(ineqop? p) (... (ineqop-iop p) 
;                             (fn-for-expr (ineqop-e1 p)) 
;                             (fn-for-expr (ineqop-e2 p)))]
;         [(logicop? p) (... (logicop-lop p)
;                              (fn-for-predicate (logicop-p1 p))
;                              (fn-for-predicate (logicop-p2 p)))]
;         [(not? p) (... (fn-for-predicate (not-p p)))]
;         [(if? p) (... (fn-for-predicate (if-cond p))
;                         (fn-for-predicate (if-then p))
;                         (fn-for-predicate (if-else p)))]))

#|
  SLTLC REFINEMENT TYPE
|#
(struct liquid-type () #:transparent)
(struct ref-type liquid-type (base predicate)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:ref-type)
        (lambda (obj) (list (ref-type-base obj) (ref-type-predicate obj)))))])
(struct ref-type-var liquid-type (name)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'lt:ref-type-var)
        (lambda (obj) (list (ref-type-var-name obj)))))])

;; Templates
; (define (fn-for-lop lop)
;   (cond [(empty? lop) ...]
;         [else (... (fn-for-predicate (first lop))
;                    (fn-for-lop (rest lop)))]))
;
; (define (fn-for-ref-type r)
;   (... (ref-type-base r)
;        (fn-for-lop (ref-type-base r))))

