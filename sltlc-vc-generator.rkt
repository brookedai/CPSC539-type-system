#lang racket
(require rosette
         (prefix-in t: "sltlc-types.rkt")
         (prefix-in lt: "sltlc-liquid-types.rkt"))

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

;; substitutes all occurrences of special var v with x
(define (substitute-v predicate x)

  (define (substitute-v-expr p x)
  (cond [(lt:arithop? p) 
         (lt:arithop (lt:arithop-aop p) 
                     (substitute-v-expr (lt:arithop-e1 p) x) 
                     (substitute-v-expr (lt:arithop-e2 p) x))]
        ;[(var? p) ... p]
        [(integer? p) p]
        [(lt:value-var? p) x]
        [else (error 'substitute-v-expr "invalid expr ~a" p)]))

  (define (substitute-v-p p x)
    (cond ;[(var? p) ( (var-name p))]
          [(lt:ineqop? p) 
           (lt:ineqop (lt:ineqop-iop p) 
                      (substitute-v-expr (lt:ineqop-e1 p) x) 
                      (substitute-v-expr (lt:ineqop-e2 p) x))]
          [(lt:logicop? p) 
           (lt:logicop (lt:logicop-lop p)
                       (substitute-v-p (lt:logicop-p1 p) x)
                       (substitute-v-p (lt:logicop-p2 p) x))]
          [(lt:not? p) (lt:not (substitute-v-p (lt:not-p p) x))]
          [(lt:if? p) (lt:if (substitute-v-p (lt:if-cond p) x)
                             (substitute-v-p (lt:if-then p) x)
                             (substitute-v-p (lt:if-else p) x))]
          [(lt:value-var? p) x]))

  (substitute-v-p predicate x))

(define-symbolic* n integer?)
(define-symbolic* x integer?)
(define rt 
  (lt:ref-type (t:int) 
               (lt:logicop 'and (lt:ineqop >= (lt:value-var) 0)
                                (lt:ineqop < (lt:value-var) n))))

(verify (generate-vc (substitute-v (lt:ref-type-predicate rt) x)))
