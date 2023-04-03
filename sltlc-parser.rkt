#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt"
         "util.rkt")
(provide parse
         parse-tests
         sltlc-val-true
         sltlc-val-false
         sltlc-val-fn-identity
         sltlc-term-app-identity
         sltlc-val-fn-double
         sltlc-val-22-5-5
         sltlc-val-zero
         sltlc-term-succ
         sltlc-term-pred)

;; Program examples
(define sltlc-val-true 'true)
(define sltlc-val-false 'false)
(define sltlc-val-fn-identity '(lambda x . x))
(define sltlc-term-app-identity `(app ,sltlc-val-fn-identity ,sltlc-val-false))
(define sltlc-val-fn-double '(lambda f . (lambda x . (app f (app f x)))))
(define sltlc-val-22-5-5 '(lambda z . (lambda y . (app z (app y true)))))
(define sltlc-val-zero '0)
(define sltlc-term-succ '(succ 0))
(define sltlc-term-pred '(pred 0))
(define sltlc-term-iszero '(iszero 0))
(define sltlc-term-if '(if true then 1 else 0))

;; Helpers
(define (parser-fresh-type-var)
  (fresh-type-var 'T))

;; SLTLC parser
;; sexpr -> sltlc or error
;; given a term t, return the AST of t if well-formed
;; otherwise, throw an error
(define (parse t)
  
  (define (is-primitive? s)
    (match s
      [x #:when (member s '(true false)) 
       #t]
      [_ #f]))

  (define (primitive-value s)
    (match s
      ['true #t]
      ['false #f]
      [_ (error 'primitive-type "invalid primitive ~a" s)]))

  ;; sexpr -> value
  (define (parse-v v)
    (match v
      [v #:when (is-primitive? v)
      (primitive-value v)]

      [n #:when (integer? n) n]

      [x (id x)]))

  ;; sexpr -> sltlc
  ;; program -> ast
  (define (parse-t t)
    (match t
      [`(lambda ,x . ,body)
       (lam x (type-var (parser-fresh-type-var)) (parse-t body))]

      [`(app ,t1 ,t2) (app (parse-t t1) (parse-t t2))]

      [`(succ ,t1) (succ (parse-t t1))]

      [`(pred ,t1) (pred (parse-t t1))]
      
      [`(iszero ,t1) (iszero (parse-t t1))]

      [`(if ,condition then ,then-b else ,else-b) 
       (if-conditional (parse-t condition) (parse-t then-b) (parse-t else-b))]

      [x (parse-v x)]
    ))
  
  (parse-t t))


;; Tests
(define parse-tests
  (test-suite "parser tests for SLTLC"
    (check-equal? (parse sltlc-val-true) #t "true")
    (check-equal? (parse sltlc-val-false) #f "false")
    (check-match (parse sltlc-val-fn-identity) (lam 'x t (id 'x)) (type-var? t))
    (check-match (parse sltlc-term-app-identity) (app (lam 'x t (id 'x)) #f) (type-var? t))
    (check-match (parse sltlc-val-fn-double) 
                 (lam 'f t1 
                   (lam 'x t2 (app (id 'f) 
                     (app (id 'f) (id 'x))))) 
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
    (check-match (parse sltlc-val-22-5-5)
                 (lam 'z t1 
                   (lam 'y t2 
                     (app (id 'z) (app (id 'y) #t))))
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
    (check-equal? (parse sltlc-val-zero) 0 "zero")
    (check-equal? (parse sltlc-term-succ) (succ 0) "succ")
    (check-equal? (parse sltlc-term-pred) (pred 0) "pred")
    (check-equal? (parse sltlc-term-iszero) (iszero 0) "iszero")
    (check-equal? (parse sltlc-term-if) (if-conditional #t 1 0) "if")
  ))

; (run-tests parse-tests)
