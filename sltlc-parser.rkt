#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt")

;; Program examples
(define val-true 'true)
(define val-false 'false)
(define val-fn-identity '(lambda x Bool x))
(define term-app-identity `(,val-fn-identity ,val-false))
(define val-fn-double '(lambda f (Bool -> Bool) (lambda x Bool (f (f x)))))

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

  ;; sexpr -> type
  (define (parse-type t)
    (match t
      ['Bool (bool)]
      [`(,t1 -> ,t2) (fun (parse-type t1) (parse-type t2))]
      [_ (error 'parse-type "parse failed for invalid type ~a" t)]))

  ;; sexpr -> sltlc
  ;; program -> ast
  (define (parse-t t)
    (match t
      [`(lambda ,x ,T ,body)
       (lam x (parse-type T) (parse-t body))]

      [`(,t1 ,t2) (app (parse-t t1) (parse-t t2))]

      [x (parse-v x)]
    ))
  
  (parse-t t))


;; Tests
(define file-tests
  (test-suite "tests for STLC with Bool"
    (check-equal? (parse val-true) #t "true")
    (check-equal? (parse val-false) #f "false")
    (check-equal? (parse val-fn-identity) (lam 'x (bool) (id 'x)) "identity")
    (check-equal? (parse term-app-identity) (app (lam 'x (bool) (id 'x)) #f) "app identity")
    (check-equal? (parse val-fn-double) 
                  (lam 'f (fun (bool) (bool)) 
                    (lam 'x (bool) (app (id 'f) 
                      (app (id 'f) (id 'x))))) 
                  "double")
  ))

(run-tests file-tests)