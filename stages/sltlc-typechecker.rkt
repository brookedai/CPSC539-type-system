#lang racket

(require racket
         rackunit
         rackunit/text-ui)
(require "../models/sltlc-ast.rkt"
         (prefix-in t: "../models/sltlc-types.rkt")
         (prefix-in lt: "../models/sltlc-liquid-types.rkt")
         "../models/sltlc-inference.rkt"
         "../util.rkt")
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

;; TODO: identity doesn't work when applied to itself
;; SLTLC AST examples
(define ast-val-true #t)
(define ast-val-false #f)
(define ast-val-fn-identity (lam 'x (lt:ref-type (t:type-var 'T1) (lt:ref-type-var 'K2)) (id 'x)))
(define ast-term-app-identity (app ast-val-fn-identity ast-val-false))
(define ast-val-fn-double (lam 'f (t:fun (lt:ref-type (t:type-var 'T5) (lt:ref-type-var 'K6)) 
                                         (lt:ref-type (t:type-var 'T7) (lt:ref-type-var 'K8)))
                            (lam 'x (lt:ref-type (t:type-var 'T9) (lt:ref-type-var 'K10))
                              (app (id 'f) (app (id 'f) (id 'x))))))
(define ast-val-22-5-5 (lam 'z (t:fun (lt:ref-type (t:type-var 'T11) (lt:ref-type-var 'K12)) 
                                                (lt:ref-type (t:type-var 'T13) (lt:ref-type-var 'K14))) 
                          (lam 'y (t:fun (lt:ref-type (t:type-var 'T15) (lt:ref-type-var 'K16)) 
                                                   (lt:ref-type (t:type-var 'T17) (lt:ref-type-var 'K18))) 
                            (app (id 'z) (app (id 'y) #t)))))
(define ast-term-app-double (app (app ast-val-fn-double ast-val-fn-identity) #t))
(define ast-val-zero 0)
(define ast-term-succ (succ 0))
(define ast-term-pred (pred 0))
(define ast-term-ineq (ineq '= 1 2))
(define ast-term-binop (binop '+ 1 2))
(define ast-term-if (if-conditional #t 1 0))
(define ast-val-div-0
  (lam 'x (lt:ref-type (t:int) #t)
    (lam 'y (lt:ref-type (t:int) (lt:ineqop '!= (id 'x) 0))
          (binop '/ (id 'x) (id 'y)))))
(define ast-term-div-0-error
  (app
    (app (lam 'x (lt:ref-type (t:int) #t)
        (lam 'y (lt:ref-type (t:int) (lt:ineqop '!= (id 'x) 0))
            (binop '/ (id 'x) (id 'y))))
        1)
    0))
(define ast-term-wrong-type-error (succ #t))

;; Helpers
(define (primitive-type v)
  (cond [(boolean? v) (t:bool)]
        [(integer? v) (t:int)]
        [else (error 'primitive-type "invalid primitive value ~a" v)]))

(define (typechecker-fresh-type-var)
  (fresh-type-var 'X))

(define (unrefined t) (lt:ref-type t #t))

(define (hm-gather-constraints p)

  ;; sexpr (dictof sexpr liquid-type) (setof sexpr) (listof constraint) -> (values type (setof sexpr) (listof constraint))
  ;; term Gamma Chi Constraints -> (type Chi^ Constraints^)
  ;; Gamma: maps var to liquid type
  ;; Chi: set of type vars in context
  ;; Constraints: list of constraints for type variables
  (define (hm-gather-constraints-s s G X C)
    (cond 
      [(id? s) (if (dict-has-key? G (id-name s))
                   (values (dict-ref G (id-name s)) X C)
                   (error 'hm-gather-constraints-s "key not found: ~a" s))]

      [(lam? s) 
        (let-values ([(t-body X-body C-body) 
                      (hm-gather-constraints-s (lam-body s) 
                                               (dict-set G (lam-param s) 
                                                           (lam-param-type s))
                                            X C)])
        (values (t:fun (lam-param-type s) t-body) X-body C-body))]
        
      [(app? s)
        ; assume that X-fun and X-arg do not share elements
        ; TODO add an assertion
        (let*-values ([(t-fun X-fun C-fun) (hm-gather-constraints-s (app-fn s) G X C)]
                      [(t-arg X-arg C-arg) (hm-gather-constraints-s (app-arg s) G X C)]
                      [(t-app) (unrefined (t:type-var (typechecker-fresh-type-var)))])
          (values t-app
                  (set-union X-fun X-arg)
                  (append (list (constraint t-fun (t:fun t-arg t-app))) C-fun C-arg)))]

      [(succ? s)
       (let-values ([(t-n X-n C-n) (hm-gather-constraints-s (succ-n s) G X C)])
         (values (unrefined (t:int))
                 X-n                                              
                 (append (list (constraint t-n (unrefined (t:int)))) C-n)))]

      [(pred? s) 
       (let-values ([(t-n X-n C-n) (hm-gather-constraints-s (pred-n s) G X C)])
         (values (unrefined (t:int)) 
                 X-n 
                 (append (list (constraint t-n (unrefined (t:int)))) C-n)))]

      [(ineq? s)
       (let-values ([(t-t1 X-t1 C-t1) (hm-gather-constraints-s (ineq-t1 s) G X C)]
                    [(t-t2 X-t2 C-t2) (hm-gather-constraints-s (ineq-t2 s) G X C)])
           (values (unrefined (t:bool))
                   (set-union X-t1 X-t2)
                   (append (list (constraint t-t1 (unrefined (t:int)))
                                 (constraint t-t2 (unrefined (t:int)))) 
                           C-t1 C-t2)))]
      
      [(binop? s)
       (let-values ([(t-t1 X-t1 C-t1) (hm-gather-constraints-s (binop-t1 s) G X C)]
                    [(t-t2 X-t2 C-t2) (hm-gather-constraints-s (binop-t2 s) G X C)])
           (values (unrefined (t:int))
                   (set-union X-t1 X-t2)
                   (append (list (constraint t-t1 (unrefined (t:int)))
                                 (constraint t-t2 (unrefined (t:int)))) 
                           C-t1 C-t2)))]

      [(if-conditional? s) 
       (let-values ([(t-cond X-cond C-cond) (hm-gather-constraints-s (if-conditional-cond s) G X C)]
                    [(t-then X-then C-then) (hm-gather-constraints-s (if-conditional-then s) G X C)]
                    [(t-else X-else C-else) (hm-gather-constraints-s (if-conditional-else s) G X C)])
           (values t-then
                   (set-union X-cond X-then X-else) 
                   (append (list (constraint t-cond (unrefined (t:bool)))
                                 (constraint t-then t-else)) 
                           (set-union C-cond C-then C-else))))]

      [(t:primitive-val? s) (values (unrefined (primitive-type s)) X C)]))
(hm-gather-constraints-s p '() (set) '()))

;; (listof constraint) (setof sexpr) -> (dictof sexpr type)
(define (unify C)
;; (listof constraint) -> (listof sexpr)
;; Constraints -> free vars
  (define (free-vars t)
    (cond [(or (t:bool? t) (t:int? t)) (set)]
          [(t:fun? t) (set-union (free-vars (t:fun-param t))
                                 (free-vars (t:fun-body t)))]
          [(t:type-var? t) (set (t:type-var-name t))]))
  
  ;; (listof constraint) sexpr type -> (listof constraint)
  ;; Constraints typevar type -> Constraints
  ;; substitutes typevar with type in all constraints
  (define (hm-substitute-x-with-y cs x y)
    (let* ([substitution (dict-set '() x y)]
           [substitute-x (lambda (c) 
                         (constraint (hm-substitute-inferred-types (constraint-S c) substitution)
                                     (hm-substitute-inferred-types (constraint-T c) substitution)))])
      (map substitute-x cs)))

  (define (hm-unify-c c)
    (match c
      [(constraint rt-S rt-T)
       #:when (and (lt:ref-type? rt-S)
                   (lt:ref-type? rt-T))
       (let ([S (lt:ref-type-base rt-S)]
             [T (lt:ref-type-base rt-T)])
         (cond 
           [(equal? S T) '()]
 
           [(and (t:type-var? S) 
                 (not (set-member? (free-vars T) (t:type-var-name S))))
            (dict-set '() (t:type-var-name S) T)]
 
           [(and (t:type-var? T) 
                 (not (set-member? (free-vars S) (t:type-var-name T))))
            (dict-set '() (t:type-var-name T) S)]
 
           [else (error 'hm-unify-cs "incompatible types: ~a ~a" S T)]))]

      [(constraint rt-S fun-T)
       #:when (and (lt:ref-type? rt-S)
                   (t:fun? fun-T))
       (let ([S-base (lt:ref-type-base rt-S)])
         (cond [(t:type-var? S-base)
           (dict-set '() (t:type-var-name S-base) fun-T)]
           [else (error 'hm-unify-cs "incompatible: base type ~a and function type ~a" S-base fun-T)]))]

      [(constraint fun-S rt-T)
       #:when (and (t:fun? fun-S)
                   (lt:ref-type? rt-T))
       (hm-unify-c (constraint rt-T fun-S))]

      [(constraint fun-S fun-T)
       #:when (and (t:fun? fun-S)
                   (t:fun? fun-T))
       (let* ([S-ref-param (t:fun-param fun-S)]
              [S-ref-body (t:fun-body fun-S)]
              [T-ref-param (t:fun-param fun-T)]
              [T-ref-body (t:fun-body fun-T)]
              [param-sigma (hm-unify-c (constraint S-ref-param T-ref-param))])
          (append 
            param-sigma
            (hm-unify-c (constraint (hm-substitute-inferred-types S-ref-body param-sigma) 
                                    (hm-substitute-inferred-types T-ref-body param-sigma)))))]

      [_ (error 'hm-unify-c "invalid constraint ~a" c)]))

  (define (hm-unify-cs cs)
    (cond [(empty? cs) '()]
          [else 
            (let* ([mappings (hm-unify-c (first cs))]
                   [substituted-rest 
                    (map (lambda (c) 
                           (constraint (hm-substitute-inferred-types (constraint-S c) mappings)
                                       (hm-substitute-inferred-types (constraint-T c) mappings))) 
                         (rest cs))])
              (append mappings (hm-unify-cs substituted-rest)))]))

  (hm-unify-cs C))

;; (oneof type liquid-type) (dictof sexpr type) -> (oneof type liquid-type)
;; type mapping -> type
;; applies mapping of (type var -> type) for the given type or liquid type.
;; does not touch the refinement predicates of liquid types.
(define (hm-substitute-inferred-types t sigma)
  (cond 
        [(or (t:bool? t) (t:int? t)) t]
        [(t:type-var? t)
         (let ([var (t:type-var-name t)])
           (if (dict-has-key? sigma var)
               (if (t:fun? (dict-ref sigma var)) 
                   (error 'hm-substitute-inferred-types "tried to substitute fun ~a into base type var ~a" (dict-ref sigma var) var)
                   (dict-ref sigma var))
               t))]
        [(t:fun? t) (t:fun (hm-substitute-inferred-types (t:fun-param t) sigma)
                           (hm-substitute-inferred-types (t:fun-body t) sigma))]
        [(lt:ref-type? t)
         (let ([base (lt:ref-type-base t)]
               [predicate (lt:ref-type-predicate t)])
           (cond [(or (t:bool? base) (t:int? base)) t]
                 [(t:type-var? base) 
                   (let ([var (t:type-var-name base)])
                     (if (dict-has-key? sigma var)
                         (if (t:fun? (dict-ref sigma var)) ; if fun, replace the entire liquid type (not just the base)
                             (dict-ref sigma var)
                             (lt:ref-type (dict-ref sigma var) predicate))
                         t))]))]))

(define (typecheck p)
  (let*-values ([(t X C) (hm-gather-constraints p)]
                [(sigma) (unify C)]
                [(substituted-sigma) (dict-map sigma (lambda (k v) `(,k . ,(hm-substitute-inferred-types v sigma))))])
    ; (pretty-print t)
    ; (for ([c C]) (pretty-print c))
    ; (pretty-print sigma)
    ; (pretty-print substituted-sigma)
    (hm-substitute-inferred-types t substituted-sigma)))

  ;; Tests
(define typecheck-tests
  (test-suite "typechecker tests for SLTLC"
    (check-equal? (typecheck ast-val-true) (unrefined (t:bool)) "true")
    (check-equal? (typecheck ast-val-false) (unrefined (t:bool)) "false")
    (check-match (typecheck ast-val-fn-identity) 
                 (t:fun lt1 lt2) 
                 (and (lt:ref-type? lt1) (lt:ref-type? lt2)))
    (check-equal? (typecheck ast-term-app-identity) (unrefined (t:bool)) "app identity")
    (check-match (typecheck ast-val-fn-double) 
                 (t:fun (t:fun lt1 lt2) 
                                  (t:fun lt3 lt4)) 
                 (and (andmap lt:ref-type? (list lt1 lt2 lt3 lt4))
                      (andmap (lambda (lt) (equal? (t:type-var-name (lt:ref-type-base lt1))
                                                   (t:type-var-name (lt:ref-type-base lt))))
                              (list lt2 lt3 lt4))))
    (check-match (typecheck ast-val-22-5-5) 
                 (t:fun (t:fun lt1 lt2)
                                  (t:fun (t:fun (lt:ref-type (t:bool) _) lt3) 
                                                   lt4))
                 (and (lt:ref-type? lt1) 
                      (lt:ref-type? lt2)
                      (equal? (t:type-var-name (lt:ref-type-base lt1))
                              (t:type-var-name (lt:ref-type-base lt3)))
                      (equal? (t:type-var-name (lt:ref-type-base lt2))
                              (t:type-var-name (lt:ref-type-base lt4)))))
    (check-equal? (typecheck ast-term-app-double) (unrefined (t:bool)) "app double")
    (check-equal? (typecheck ast-term-succ) (unrefined (t:int)) "succ")
    (check-equal? (typecheck ast-term-pred) (unrefined (t:int)) "pred")
    (check-equal? (typecheck ast-term-ineq) (unrefined (t:bool)) "ineq")
    (check-equal? (typecheck ast-term-binop) (unrefined (t:int)) "ineq")
    (check-equal? (typecheck ast-term-if) (unrefined (t:int)) "if")
    (check-equal? (typecheck ast-val-div-0) 
                  (t:fun (lt:ref-type (t:int) #t)
                         (t:fun (lt:ref-type (t:int) (lt:ineqop '!= (id 'x) 0)) 
                                (lt:ref-type (t:int) #t))) 
                  "lam div 0")
    (check-equal? (typecheck ast-term-div-0-error) (unrefined (t:int)) "app div 0 error")
    (check-exn exn:fail? (lambda () (typecheck ast-term-wrong-type-error)))
  ))

; (run-tests typecheck-tests)