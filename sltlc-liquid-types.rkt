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
(struct predicate ())
(struct var predicate (name))
(struct ineqop predicate (iop e1 e2))
(struct logicop predicate (lop p1 p2))
(struct arithop (aop e1 e2))
(struct not (p))
(struct if (cond then else))

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
;         [(not? p) (... (not-p p))]
;         [(if? p) (... (fn-for-predicate (if-cond p))
;                         (fn-for-predicate (if-then p))
;                         (fn-for-predicate (if-else p)))]))

#|
  SLTLC REFINEMENT TYPE
|#
(struct liquid-type ())
(struct ref-type liquid-type (base predicates))
(struct ref-type-var liquid-type (base predicates))

;; Templates
; (define (fn-for-lop lop)
;   (cond [(empty? lop) ...]
;         [else (... (fn-for-predicate (first lop))
;                    (fn-for-lop (rest lop)))]))
;
; (define (fn-for-ref-type r)
;   (... (ref-type-base r)
;        (fn-for-lop (ref-type-base r))))

