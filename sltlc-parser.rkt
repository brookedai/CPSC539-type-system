#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt"
         "util.rkt")
(provide parse
         parse-tests)

;; Program examples
(define val-true 'true)
(define val-false 'false)
(define val-fn-identity '(lambda x . x))
(define term-app-identity `(,val-fn-identity ,val-false))
(define val-fn-double '(lambda f . (lambda x . (f (f x)))))
(define val-22-5-5 '(lambda z . (lambda y . (z (y true)))))

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
      [v
      #:when (is-primitive? v)
      (primitive-value v)]

      [x (id x)]))

  ;; sexpr -> sltlc
  ;; program -> ast
  (define (parse-t t)
    (match t
      [`(lambda ,x . ,body)
       (lam x (type-var (parser-fresh-type-var)) (parse-t body))]

      [`(,t1 ,t2) (app (parse-t t1) (parse-t t2))]

      [x (parse-v x)]
    ))
  
  (parse-t t))


;; Tests
(define parse-tests
  (test-suite "parser tests for SLTLC"
    (check-equal? (parse val-true) #t "true")
    (check-equal? (parse val-false) #f "false")
    (check-match (parse val-fn-identity) (lam 'x t (id 'x)) (type-var? t))
    (check-match (parse term-app-identity) (app (lam 'x t (id 'x)) #f) (type-var? t))
    (check-match (parse val-fn-double) 
                 (lam 'f t1 
                   (lam 'x t2 (app (id 'f) 
                     (app (id 'f) (id 'x))))) 
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
    (check-match (parse val-22-5-5)
                 (lam 'z t1 
                   (lam 'y t2 
                     (app (id 'z) (app (id 'y) #t))))
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
  ))

(run-tests parse-tests)

