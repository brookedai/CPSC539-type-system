#lang racket
;; Referencing CPSC 311

(require racket
         racket/struct)
(require "sltlc-types.rkt")
(provide (all-defined-out))

(struct sltlc ())
(struct id (name)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'id)
        (lambda (obj) (list (id-name obj)))))])
(struct lam (param param-type body)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
    (lambda (obj) 'lam)
    (lambda (obj) (list (lam-param obj) (lam-param-type obj) (lam-body obj)))))])
(struct app (fn arg)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'app)
        (lambda (obj) (list (app-fn obj) (app-arg obj)))))])

;; Template
; (define (fn-for-sltlc s)
;   (cond [(id? s) (... s)]
;         [(lam? s) (... (lam-param s)
;                        (fn-for-type (lam-param-type s))
;                        (fn-for-sltlc (lam-body s)))]
;         [(app? s) (... (fn-for-sltlc (app-fn s))
;                        (fn-for-sltlc (app-arg s)))]))

;; Examples
(define x (id 'x))
(define fn-identity (lam 'x (bool) (id 'x)))
