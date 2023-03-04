#lang racket

(require racket)
(require rackunit
         rackunit/text-ui)

#|
t ::=                   terms:
  x                     variable
  (lambda x T t)        abstraction
  (t t)                 application
v ::=                   values:
  true | false          boolean constants
  (lambda x T t)        abstraction value
T ::=                   types:
  Bool                  boolean
  T -> T                type of functions
G ::=                   contexts:
  '()                   empty context
  (dict-set G x T)      term variable binding
|#

;; Program examples
(define val-true 'true)
(define val-false 'false)
(define val-fn-identity '(lambda x Bool x))
(define term-app-identity `(,val-fn-identity ,val-false))
(define val-fn-double '(lambda f (Bool -> Bool) (lambda x Bool (f (f x)))))

;; Context examples
(define empty-gamma #hash())
(define gamma-1 '((x . Bool)))

;; Type checker
;; sexpr -> sexpr or error
;; given a term t, return the type of t if t is well-typed
;; otherwise, throw an error
(define (type-check t)
  
  (define (is-primitive? s)
    (match s
      ['true #t]
      ['false #t]
      [_ #f]))

  (define (primitive-type s)
    (match s
      ['true 'Bool]
      ['false 'Bool]
      [_ (error 'primitive-type "invalid primitive ~a" s)]))

  ;; sexpr dict -> sexpr
  (define (type-check-v v G)
    (match v
      [v
      #:when (is-primitive? v)
      (primitive-type v)]

      [x 
      (if (dict-has-key? G x)
            (dict-ref G x)
            (error 'type-check "key not found: ~a" x))]))

  ;; sexpr dict -> sexpr
  (define (type-check-t t G)
    (match t
      [`(lambda ,x ,T ,body)
       (let ([T-body (type-check-t body (dict-set G x T))])
         `(,T -> ,T-body))]

      [`(,t1 ,t2)
      (let* ([T-t1 (type-check-t t1 G)]
             [T-t2 (type-check-t t2 G)])
        (match T-t1
          [`(,T-x -> ,T-body)
           (if (eq? T-x T-t2)
             T-body
             (error 'type-check-t "type mismatch: ~a =/= ~a" T-t1 T-t2))]
          [_ (error 'type-check-t "invalid type for application t1: ~a" T-t1)]))]

      [x (type-check-v x G)]
    ))
  
  (type-check-t t '()))

;; Interpreter
(define (interpret t)
  (error "not implemented yet!"))

;; Tests
(define file-tests
  (test-suite "tests for STLC with Bool"
    (check-equal? (type-check val-true) 'Bool "true")
    (check-equal? (type-check val-false) 'Bool "false")
    (check-equal? (type-check val-fn-identity) '(Bool -> Bool) "identity")
    (check-equal? (type-check term-app-identity) 'Bool "app identity")
    (check-equal? (type-check val-fn-double) '((Bool -> Bool) -> (Bool -> Bool)))
  ))

(run-tests file-tests)
;; TODO type equality