#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt")

;; SLTLC AST examples
(define val-true #t)
(define val-false #f)
(define val-fn-identity (lam 'x (bool) (id 'x)))
(define term-app-identity (app val-fn-identity val-false))
(define val-fn-double (lam 'f (fun (bool) (bool)) 
                        (lam 'x (bool) 
                          (app (id 'f) (app (id 'f) (id 'x))))))


(define (typecheck p)
  (define (primitive-type v)
    (cond [(boolean? v) (bool)]
          [else (error 'primitive-type "invalid primitive value ~a" v)]))


  (define (typecheck-sltlc s G)
    (cond [(id? s) (if (dict-has-key? G (id-name s))
                       (dict-ref G (id-name s))
                       (error 'typecheck-sltlc "key not found: ~a" s))]
          [(lam? s) (fun (lam-param-type s)
                         (typecheck-sltlc (lam-body s) 
                                          (dict-set G (lam-param s) 
                                                      (lam-param-type s))))]
          [(app? s) 
           (let* ([T-fn  (typecheck-sltlc (app-fn s) G)]
                  [T-arg (typecheck-sltlc (app-arg s) G)])
              (cond [(fun? T-fn) 
                     (if (equal? (fun-param T-fn) T-arg) 
                         (fun-body T-fn)
                         (error 'typecheck-sltlc "type mismatch: ~a =/= ~a" (fun-param T-fn) T-arg))]
                    [else (error 'typecheck-sltlc "invalid type for application t1: ~a" T-fn)]))]
          [(primitive-val? s) (primitive-type s)]))
  (typecheck-sltlc p '()))

  ;; Tests
(define file-tests
  (test-suite "typechecker tests for SLTLC"
    (check-equal? (typecheck val-true) (bool) "true")
    (check-equal? (typecheck val-false) (bool) "false")
    (check-equal? (typecheck val-fn-identity) (fun (bool) (bool)) "identity")
    (check-equal? (typecheck term-app-identity) (bool) "app identity")
    (check-equal? (typecheck val-fn-double) (fun (fun (bool) (bool)) (fun (bool) (bool))) "double")
  ))

(run-tests file-tests)