#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-ast.rkt"
         "sltlc-types.rkt"
         "sltlc-inference.rkt"
         "util.rkt")
(provide typecheck
         typecheck-tests
         ast-val-true
         ast-val-false
         ast-val-fn-identity
         ast-term-app-identity
         ast-val-fn-double
         ast-val-22-5-5
         ast-val-zero
         ast-term-succ
         ast-term-pred)

;; SLTLC AST examples
(define ast-val-true #t)
(define ast-val-false #f)
(define ast-val-fn-identity (lam 'x (type-var 'T1) (id 'x)))
(define ast-term-app-identity (app ast-val-fn-identity ast-val-false))
(define ast-val-fn-double (lam 'f (type-var 'T2)
                            (lam 'x (type-var 'T3)
                              (app (id 'f) (app (id 'f) (id 'x))))))
(define ast-val-22-5-5 (lam 'z (type-var 'T4) 
                        (lam 'y (type-var 'T5) 
                          (app (id 'z) (app (id 'y) #t)))))
(define ast-term-app-double (app (app ast-val-fn-double ast-val-fn-identity) #t))
(define ast-val-zero 0)
(define ast-term-succ (succ 0))
(define ast-term-pred (pred 0))
(define ast-term-iszero (iszero 0))
(define ast-term-if (if-conditional #t 1 0))

;; Helpers
(define (primitive-type v)
  (cond [(boolean? v) (bool)]
        [(integer? v) (int)]
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
    (cond 
      [(id? s) (if (dict-has-key? G (id-name s))
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

      [(succ? s)
       (let-values ([(t-n X-n C-n) (gather-constraints-s (succ-n s) G X C)])
         (values (int) 
                 X-n 
                 (append (list (constraint t-n (int))) C-n)))]

      [(pred? s) 
       (let-values ([(t-n X-n C-n) (gather-constraints-s (pred-n s) G X C)])
         (values (int) 
                 X-n 
                 (append (list (constraint t-n (int))) C-n)))]

      [(iszero? s)
       (let-values ([(t-n X-n C-n) (gather-constraints-s (iszero-n s) G X C)])
           (values (bool) 
                   X-n 
                   (append (list (constraint t-n (int))) C-n)))]

      [(if-conditional? s) 
       (let-values ([(t-cond X-cond C-cond) (gather-constraints-s (if-conditional-cond s) G X C)]
                    [(t-then X-then C-then) (gather-constraints-s (if-conditional-then s) G X C)]
                    [(t-else X-else C-else) (gather-constraints-s (if-conditional-else s) G X C)])
           (values t-then
                   (set-union X-cond X-then X-else) 
                   (append (list (constraint t-cond (bool))
                                 (constraint t-then t-else)) 
                           (set-union C-cond C-then C-else))))]

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
  (cond [(or (bool? t) (int? t)) t]
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
    (check-equal? (typecheck ast-val-true) (bool) "true")
    (check-equal? (typecheck ast-val-false) (bool) "false")
    (check-match (typecheck ast-val-fn-identity) (fun t t) (type-var? t))
    (check-equal? (typecheck ast-term-app-identity) (bool) "app identity")
    (check-match (typecheck ast-val-fn-double) (fun (fun t t) (fun t t)) (type-var? t))
    (check-match (typecheck ast-val-22-5-5) 
                 (fun (fun t1 t2)
                   (fun (fun (bool) t1) t2))
                 (and (type-var? t1) (type-var? t2) (unique? (list t1 t2))))
    (check-equal? (typecheck ast-term-app-double) (bool) "app double")
    (check-equal? (typecheck ast-val-zero) (int) "zero")
    (check-equal? (typecheck ast-term-succ) (int) "succ")
    (check-equal? (typecheck ast-term-pred) (int) "pred")
    (check-equal? (typecheck ast-term-iszero) (bool) "iszero")
    (check-equal? (typecheck ast-term-if) (int) "if")
  ))

; (run-tests typecheck-tests)
