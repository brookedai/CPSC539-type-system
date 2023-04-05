#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "../models/sltlc-ast.rkt"
         (prefix-in t: "../models/sltlc-types.rkt")
         (prefix-in lt: "../models/sltlc-liquid-types.rkt")
         "../util.rkt")
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
         sltlc-term-pred
         sltlc-term-ineq
         sltlc-term-binop
         sltlc-term-if
         sltlc-val-div-0
         sltlc-term-div-0-error)

;; Program examples
(define sltlc-val-true 'true)
(define sltlc-val-false 'false)
(define sltlc-val-fn-identity '(lambda x auto x))
(define sltlc-term-app-identity `(app ,sltlc-val-fn-identity ,sltlc-val-false))
(define sltlc-val-fn-double '(lambda f (auto -> auto) (lambda x auto (app f (app f x)))))
(define sltlc-val-22-5-5 '(lambda z (auto -> auto) (lambda y (auto -> auto) (app z (app y true)))))
(define sltlc-val-zero '0)
(define sltlc-term-succ '(succ 0))
(define sltlc-term-pred '(pred 0))
(define sltlc-term-ineq '(= 1 2))
(define sltlc-term-binop '(+ 1 2))
(define sltlc-term-if '(if true then 1 else 0))
(define sltlc-val-div-0 '(lambda x (Int true) (lambda y (Int (!= x 0)) (/ x y))))
(define sltlc-term-div-0-error `(app (app ,sltlc-val-div-0 1) 0))

;; Helpers
(define (parser-fresh-type-var)
  (fresh-type-var 'T))
(define (parser-fresh-lt-var)
  (fresh-type-var 'K))

;; SLTLC parser
;; sexpr -> sltlc or error
;; given a term t, return the AST of t if well-formed
;; otherwise, throw an error
(define (parse t)
  (define iops '(< <= = >= > !=))
  (define laops '(+ -))
  (define aops '(+ - * /))
  (define lops '(and or))
  (define (iop? o) (member o iops))
  (define (laop? o) (member o laops))
  (define (aop? o) (member o aops))
  (define (lop? o) (member o lops))
  
  (define (primitive-val? s)
    (match s
      [x #:when (or (member s '(true false)) (integer? s)) 
       #t]
      [_ #f]))

  (define (primitive-value s)
    (match s
      ['true #t]
      ['false #f]
      [n #:when (integer? n) n]
      [_ (error 'primitive-type "invalid primitive ~a" s)]))
  
  (define (parse-expr e)
    (match e
      [`(,laop ,e1 ,e2) #:when (laop? laop)
       (lt:arithop laop (parse-expr e1) (parse-expr e2))]
      [n #:when (integer? n) n]
      [x (id x)]))

  (define (parse-q q)
    (match q
           [v #:when (primitive-val? v) 
            (primitive-value v)]
           [`(,iop ,e1 ,e2) #:when (iop? iop)
            (lt:ineqop iop (parse-expr e1) (parse-expr e2))]
           [`(,lop ,q1 ,q2) #:when (lop? lop)
            (lt:logicop lop (parse-q q1) (parse-q q2))]))

  ;; sexpr -> (listof predicate)
  (define (parse-p p)
    (match p
      ['k (lt:ref-type-var (parser-fresh-lt-var))]

      [q (parse-q q)]))

  ;; sexpr -> type
  (define (parse-type t)
    (match t
      ['Bool (t:bool)]

      ['Int (t:int)]

      [_ (error 'parse-type "invalid type: ~a" t)]))

  ;; sexpr -> liquid-type
  (define (parse-lt lt)
    (match lt
      ['auto (lt:ref-type (t:type-var (parser-fresh-type-var))
                          (lt:ref-type-var (parser-fresh-lt-var)))]

      [`(,B ,p) (lt:ref-type (parse-type B)
                          (parse-p p))]

      [`(,T1 -> ,T2) (t:fun (parse-lt T1)
                            (parse-lt T2))]
      
      [_ (error 'parse-lt "invalid liquid type: ~a" lt)]))

  ;; sexpr -> value
  (define (parse-v v)
    (match v
      [v #:when (primitive-val? v)
      (primitive-value v)]

      [n #:when (integer? n) n]

      [x (id x)]))

  ;; sexpr -> sltlc
  ;; program -> ast
  (define (parse-t t)
    (match t
      [`(lambda ,x ,T ,body)
       (lam x (parse-lt T) (parse-t body))]

      [`(app ,t1 ,t2) (app (parse-t t1) (parse-t t2))]

      [`(succ ,t1) (succ (parse-t t1))]

      [`(pred ,t1) (pred (parse-t t1))]
      
      [`(,iop ,t1 ,t2) #:when (iop? iop)
       (ineq iop (parse-t t1) (parse-t t2))]

      [`(,aop ,t1 ,t2) #:when (aop? aop)
       (binop aop (parse-t t1) (parse-t t2))]

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
    (check-match (parse sltlc-val-fn-identity) 
                 (lam 'x (lt:ref-type tv ltv) (id 'x)) 
                 (and (t:type-var? tv) (lt:ref-type-var? ltv)))
    (check-match (parse sltlc-term-app-identity) 
                 (app (lam 'x (lt:ref-type tv ltv) (id 'x)) #f) 
                 (and (t:type-var? tv) (lt:ref-type-var? ltv)))
    (check-match (parse sltlc-val-fn-double) 
                 (lam 'f (t:fun lt1 lt2)
                   (lam 'x (lt:ref-type tv ltv) 
                     (app (id 'f) 
                       (app (id 'f) (id 'x))))) 
                 (and (lt:ref-type? lt1)
                      (lt:ref-type? lt2)  
                      (t:type-var? tv)
                      (lt:ref-type-var? ltv)))
    (check-match (parse sltlc-val-22-5-5)
                 (lam 'z lt1 
                   (lam 'y lt2 
                     (app (id 'z) (app (id 'y) #t))))
                 (and (t:fun? lt1) 
                      (t:fun? lt2)))
    (check-equal? (parse sltlc-val-zero) 0 "zero")
    (check-equal? (parse sltlc-term-succ) (succ 0) "succ")
    (check-equal? (parse sltlc-term-pred) (pred 0) "pred")
    (check-equal? (parse sltlc-term-ineq) (ineq '= 1 2) "ineq")
    (check-equal? (parse sltlc-term-binop) (binop '+ 1 2) "binop")
    (check-equal? (parse sltlc-term-if) (if-conditional #t 1 0) "if")
    (check-equal? (parse sltlc-val-div-0) 
                  (lam 'x (lt:ref-type (t:int) #t)
                    (lam 'y (lt:ref-type (t:int) (lt:ineqop '!= (id 'x) 0))
                         (binop '/ (id 'x) (id 'y)))))
    (check-equal? (parse sltlc-term-div-0-error) 
                  (app
                    (app (lam 'x (lt:ref-type (t:int) #t)
                        (lam 'y (lt:ref-type (t:int) (lt:ineqop '!= (id 'x) 0))
                            (binop '/ (id 'x) (id 'y))))
                       1)
                    0))
  ))

; (run-tests parse-tests)