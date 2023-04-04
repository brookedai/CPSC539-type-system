#lang racket
(require rosette
         (prefix-in t: "sltlc-types.rkt")
         (prefix-in lt: "sltlc-liquid-types.rkt"))

;; (listof predicate) -> (void)
;; calls rosette's `assert` for each of the given predicates
;; note: call (clear-vc!) after this to clear the assertions
(define (generate-vc predicates)

  (define (generate-vc-expr p)
  (cond [(lt:arithop? p) ((lt:arithop-aop p) 
                       (generate-vc-expr (lt:arithop-e1 p)) 
                       (generate-vc-expr (lt:arithop-e2 p)))]
        ;[(var? p) ... p]
        [(integer? p) p]
        [else (error 'fn-for-expr "invalid expr ~a" p)]))

  (define (generate-vc-p p)
    (cond ;[(var? p) ( (var-name p))]
          [(lt:ineqop? p) ((lt:ineqop-iop p) 
                        (generate-vc-expr (lt:ineqop-e1 p)) 
                        (generate-vc-expr (lt:ineqop-e2 p)))]
          [(lt:logicop? p) ((lt:logicop-lop p)
                         (generate-vc-p (lt:logicop-p1 p))
                         (generate-vc-p (lt:logicop-p2 p)))]
          [(lt:not? p) (! (lt:not-p p))]
          [(lt:if? p) (&& (=> (generate-vc-p (lt:if-cond p))
                           (generate-vc-p (lt:if-then p)))
                       (=> (! (generate-vc-p (lt:if-cond p)))
                           (generate-vc-p (lt:if-else p))))]))

  (define (generate-vc-lop lop)
    (cond [(empty? lop) #t]
          [else (&& (generate-vc-p (first lop))
                    (generate-vc-lop (rest lop)))]))

  (assert (generate-vc-lop predicates)))

(define-symbolic* n integer?)
(define-symbolic* x integer?)
(define rt 
  (lt:ref-type (t:int) 
               (list (lt:ineqop >= x 0)
                     (lt:ineqop < x n))))
(verify (generate-vc (lt:ref-type-predicates rt)))
