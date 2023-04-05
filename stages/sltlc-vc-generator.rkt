#lang racket
(require rosette
         (prefix-in t: "../models/sltlc-types.rkt")
         (prefix-in lt: "../models/sltlc-liquid-types.rkt")
         "../models/sltlc-ast.rkt")

; (define (generate-Q)
;   (map (lambda (iop) (lt:ineqop iop (lt:value-var) 0))
;        (list '< '<= '= '>= '> '!=)))
; (generate-Q)

;; sltlc-ast -> sltlc-ast
; (define (generate-predicates s)

;   ;; sltlc-ast (mapof sexpr liquid-type) (setof guard predicates) -> 
;   (define (generate-predicates-p e G-tb G-gp)
;     (cond 
;         [(id? s) (... s)]
;         [(lam? s) (... (lam-param s)
;                        (fn-for-type (lam-param-type s))
;                        (fn-for-sltlc (lam-body s)))]
;         [(app? s) (... (fn-for-sltlc (app-fn s))
;                        (fn-for-sltlc (app-arg s)))]
;         [(succ? s) (... (succ-n s))]
;         [(pred? s) (... (succ-n s))]
;         [(iszero? s) (... (succ-n s))]
;         [(if-conditional? s) (... (fn-for-sltlc (if-conditional-cond s))
;                                   (fn-for-sltlc (if-conditional-then s))
;                                   (fn-for-sltlc (if-conditional-else s)))]))

;   (generate-predicates-p s '())

;; (listof predicate) -> (void)
;; calls rosette's `assert` for each of the given predicates
;; note: call (clear-vc!) after this to clear the assertions
(define (generate-vc predicate)

  (define (generate-vc-expr p)
  (cond [(lt:arithop? p) ((lt:arithop-aop p) 
                       (generate-vc-expr (lt:arithop-e1 p)) 
                       (generate-vc-expr (lt:arithop-e2 p)))]
        ;[(var? p) ... p]
        [(integer? p) p]
        [else (error 'generate-vc-expr "invalid expr ~a" p)]))

  (define (generate-vc-p p)
    (cond ;[(var? p) ( (var-name p))]
          [(lt:ineqop? p) ((lt:ineqop-iop p) 
                        (generate-vc-expr (lt:ineqop-e1 p)) 
                        (generate-vc-expr (lt:ineqop-e2 p)))]
          [(lt:logicop? p) 
           (match (lt:logicop-lop p)
            ['and (&& (generate-vc-p (lt:logicop-p1 p))
                     (generate-vc-p (lt:logicop-p2 p)))]
            ['or (|| (generate-vc-p (lt:logicop-p1 p))
                         (generate-vc-p (lt:logicop-p2 p)))])]
          [(lt:not? p) (! (generate-vc-p (lt:not-p p)))]
          [(lt:if? p) (&& (=> (generate-vc-p (lt:if-cond p))
                           (generate-vc-p (lt:if-then p)))
                       (=> (! (generate-vc-p (lt:if-cond p)))
                           (generate-vc-p (lt:if-else p))))]))

  (assert (generate-vc-p predicate)))

;; substitutes all occurrences of variable v with x
;; v is oneof (lt:value-var), (id sexpr)
(define (substitute-v predicate v x)

  (define (substitute-v-expr p)
  (cond [(equal? v p) x]
        [(lt:arithop? p) 
         (lt:arithop (lt:arithop-aop p) 
                     (substitute-v-expr (lt:arithop-e1 p)) 
                     (substitute-v-expr (lt:arithop-e2 p)))]
        ;[(var? p) ... p]
        [(integer? p) p]
        [else (error 'substitute-v-expr "invalid expr ~a" p)]))

  (define (substitute-v-p p)
    (cond ;[(var? p) ( (var-name p))]
          [(lt:ineqop? p) 
           (lt:ineqop (lt:ineqop-iop p) 
                      (substitute-v-expr (lt:ineqop-e1 p)) 
                      (substitute-v-expr (lt:ineqop-e2 p)))]
          [(lt:logicop? p) 
           (lt:logicop (lt:logicop-lop p)
                       (substitute-v-p (lt:logicop-p1 p))
                       (substitute-v-p (lt:logicop-p2 p)))]
          [(lt:not? p) (lt:not (substitute-v-p (lt:not-p p)))]
          [(lt:if? p) (lt:if (substitute-v-p (lt:if-cond p))
                             (substitute-v-p (lt:if-then p))
                             (substitute-v-p (lt:if-else p)))]
          [(lt:value-var? p) x]))

  (substitute-v-p predicate))

(define-symbolic* n integer?)
(define-symbolic* x integer?)
(define rt 
  (lt:ref-type (t:int) 
               (lt:logicop 'and (lt:ineqop >= (lt:value-var) 0)
                                (lt:ineqop < (lt:value-var) n))))

; (verify (generate-vc (substitute-v (lt:ref-type-predicate rt) x)))

; (clear-vc!)
(define rt2 
  (lt:ref-type (t:int)
               (lt:logicop 'and (lt:ineqop >= (id 'x) 0)
                                (lt:ineqop < (id 'x) n))))

(define sub (substitute-v (lt:ref-type-predicate rt2) (id 'x) 1))
(verify (generate-vc sub))