#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt"
         "sltlc-inference.rkt"
         "util.rkt")
(provide typecheck
         typecheck-tests)

;; SLTLC AST examples
(define val-true #t)
(define val-false #f)
(define val-fn-identity (lam 'x (type-var 'T1) (id 'x)))
(define term-app-identity (app val-fn-identity val-false))
(define val-fn-double (lam 'f (type-var 'T2)
                        (lam 'x (type-var 'T3)
                          (app (id 'f) (app (id 'f) (id 'x))))))
(define val-22-5-5 (lam 'z (type-var 'T4) 
                     (lam 'y (type-var 'T5) 
                       (app (id 'z) (app (id 'y) #t)))))
(define term-app-double (app (app val-fn-double val-fn-identity) #t))

;; Helpers
(define (primitive-type v)
  (cond [(boolean? v) (bool)]
        [else (error 'primitive-type "invalid primitive value ~a" v)]))

(define (typechecker-fresh-type-var)
  (fresh-type-var 'X))

(define (gather-constraints p)

  ;; sexpr (dictof sexpr type) (setof sexpr) (listof constraint) -> (values type (setof sexpr) (listof constraint))
  ;; term Gamma Chi Constraints -> (type Chi^ Constraints^)
  ;; Gamma: maps var to type
  ;; Chi: set of type vars in context
  ;; Constraints: list of constraints for type variables
  (define (gather-constraints-s s G X C)
    (cond [(id? s) (if (dict-has-key? G (id-name s))
                       (values (dict-ref G (id-name s)) X C)
                       (error 'gather-constraints-s "key not found: ~a" s))]
          [(lam? s) 
           (let-values ([(t-body X-body C-body) 
                         (gather-constraints-s (lam-body s) 
                                               (dict-set G (lam-param s) 
                                                           (lam-param-type s))
                                               X C)])
            (values (fun (lam-param-type s) t-body) X-body C-body))]
          [(app? s)
            ; assume that X-fun and X-arg do not share elements
            ; TODO add an assertion
            (let*-values ([(t-fun X-fun C-fun) (gather-constraints-s (app-fn s) G X C)]
                          [(t-arg X-arg C-arg) (gather-constraints-s (app-arg s) G X C)]
                          [(t-app) (type-var (typechecker-fresh-type-var))])
              (values t-app
                      (set-union X-fun X-arg)
                      (append (list (constraint t-fun (fun t-arg t-app))) C-fun C-arg)))]
          [(primitive-val? s) (values (primitive-type s) X C)]))
(gather-constraints-s p '() (set) '()))

;; (listof constraint) (setof sexpr) -> (dictof sexpr type)
(define (unify C)
;; (listof constraint) -> (listof sexpr)
;; Constraints -> free vars
  (define (free-vars t)
    (cond [(bool? t) (set)]
          [(fun? t) (set-union (free-vars (fun-param t))
                               (free-vars (fun-body t)))]
          [(type-var? t) (set (type-var-name t))]))
  
  ;; (listof constraint) sexpr type -> (listof constraint)
  ;; Constraints typevar type -> Constraints
  ;; substitutes typevar with type in all constraints
  (define (substitute-x-with-y cs x y)
    (let* ([substitution (dict-set '() x y)]
           [substitute-x (lambda (c) 
                         (constraint (substitute-inferred-types (constraint-S c) substitution)
                                     (substitute-inferred-types (constraint-T c) substitution)))])
      (map substitute-x cs)))

  (define (unify-cs cs)
    (match cs 
      ['() '()]
      [(cons c cs^)
        (let ([S (constraint-S c)]
              [T (constraint-T c)])
          (cond 
            [(equal? S T) (unify-cs cs^)]

            [(and (type-var? S) 
                  (not (set-member? (free-vars T) (type-var-name S))))
            (let* ([S-name (type-var-name S)]
                    [substituted-cs^ (substitute-x-with-y cs^ S-name T)])
              (dict-set (unify-cs substituted-cs^) S-name T))]

            [(and (type-var? T) 
                  (not (set-member? (free-vars S) (type-var-name T))))
            (let* ([T-name (type-var-name T)]
                    [substituted-cs^ (substitute-x-with-y cs^ T-name S)])
              (dict-set (unify-cs substituted-cs^) T-name S))]

            [(and (fun? S) (fun? T))
            (let* ([S-param (fun-param S)]
                  [S-body  (fun-body S)]
                  [T-param (fun-param T)]
                  [T-body  (fun-body T)]
                  [param-sigma (unify-cs (list (constraint S-param T-param)))])
              (unify-cs (append (list (constraint S-param T-param)
                                      (constraint (substitute-inferred-types S-body param-sigma) 
                                            (substitute-inferred-types T-body param-sigma)))

                                cs^)))]

            [else (error 'unify-cs "incompatible types: ~a ~a" S T)]))]))

  (unify-cs C))

;; type (dictof sexpr type) -> type
;; type mapping -> type
;; applies mapping of (type var -> type) for the given type
(define (substitute-inferred-types t sigma)
  (cond [(bool? t) t]
        [(fun? t)  (fun (substitute-inferred-types (fun-param t) sigma)
                        (substitute-inferred-types (fun-body t) sigma))]
        [(type-var? t) 
         (let ([var (type-var-name t)])
           (if (dict-has-key? sigma var)
               (dict-ref sigma var)
               t))]))

(define (typecheck p)
  (let*-values ([(t X C) (gather-constraints p)]
                [(sigma) (unify C)]
                [(substituted-sigma) (dict-map sigma (lambda (k v) `(,k . ,(substitute-inferred-types v sigma))))])
    ; (display (format "~a\n" t))
    ; (display (format "~a\n" sigma))
    ; (display (format "~a\n" substituted-sigma))
    (substitute-inferred-types t substituted-sigma)))

  ;; Tests
(define typecheck-tests
  (test-suite "typechecker tests for SLTLC"
    (check-equal? (typecheck val-true) (bool) "true")
    (check-equal? (typecheck val-false) (bool) "false")
    (check-match (typecheck val-fn-identity) (fun t t) (type-var? t))
    (check-equal? (typecheck term-app-identity) (bool) "app identity")
    (check-match (typecheck val-fn-double) (fun (fun t t) (fun t t)) (type-var? t))
    (check-match (typecheck val-22-5-5) 
                 (fun (fun t1 t2)
                   (fun (fun (bool) t1) t2))
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
    (check-equal? (typecheck term-app-double) (bool) "app double")
  ))

(run-tests typecheck-tests)
