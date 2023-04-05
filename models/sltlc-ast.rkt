#lang racket
;; Referencing CPSC 311

(require racket
         racket/struct)
(require "sltlc-types.rkt")
(provide (all-defined-out))

(struct sltlc () #:transparent)
(struct id sltlc (name)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'id)
        (lambda (obj) (list (id-name obj)))))])
(struct lam sltlc (param param-type body)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
    (lambda (obj) 'lam)
    (lambda (obj) (list (lam-param obj) (lam-param-type obj) (lam-body obj)))))])
(struct app sltlc (fn arg)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'app)
        (lambda (obj) (list (app-fn obj) (app-arg obj)))))])
(struct succ sltlc (n)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'succ)
        (lambda (obj) (list (succ-n obj)))))])
(struct pred sltlc (n)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'pred)
        (lambda (obj) (list (pred-n obj)))))])
(struct ineq sltlc (iop t1 t2)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'ineq)
        (lambda (obj) (list (ineq-iop obj) (ineq-t1 obj) (ineq-t2 obj)))))])
(struct binop sltlc (aop t1 t2)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'binop)
        (lambda (obj) (list (binop-aop obj) (binop-t1 obj) (binop-t2 obj)))))])
(struct if-conditional sltlc (cond then else)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'if)
        (lambda (obj) (list (if-conditional-cond obj) (if-conditional-then obj) (if-conditional-else obj)))))])

;; Template
; (define (fn-for-sltlc s)
;   (cond [(id? s) (... s)]
;         [(lam? s) (... (lam-param s)
;                        (fn-for-type (lam-param-type s))
;                        (fn-for-sltlc (lam-body s)))]
;         [(app? s) (... (fn-for-sltlc (app-fn s))
;                        (fn-for-sltlc (app-arg s)))]
;         [(succ? s) (... (succ-n s))]
;         [(pred? s) (... (succ-n s))]
;         [(ineq? s) (... (ineq-iop s) 
;                         (fn-for-sltlc (ineq-t1 s))
;                         (fn-for-sltlc (ineq-t2 s)))]
;         [(binop? s) (... (binop-aop s) (binop-t1 s) (binop-t2 s))]
;         [(if-conditional? s) (... (fn-for-sltlc (if-conditional-cond s))
;                                   (fn-for-sltlc (if-conditional-then s))
;                                   (fn-for-sltlc (if-conditional-else s)))]))

