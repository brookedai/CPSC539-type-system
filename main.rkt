#lang plai

(require racket
         rackunit
         rackunit/text-ui)
(require "sltlc-parser.rkt"
         "sltlc-typechecker.rkt")

#|
SIMPLE LIQUID-TYPED LAMBDA CALCULUS (SLTLC)
Surface syntax
t ::=                   terms:
  x                     variable
  (lambda x . t)        abstraction
  (t t)                 application
v ::=                   values:
  true | false          boolean constants
  (lambda x . t)        abstraction value
T ::=                   types:
  Bool                  boolean
  T -> T                type of functions

Context
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

(define (run-tests)
  (run-tests parse-tests)
  (run-tests typecheck-tests))

(define (main)
  (println (typecheck (parse val-true)))
  (println (typecheck (parse val-false)))
  (println (typecheck (parse val-fn-identity)))
  (println (typecheck (parse term-app-identity)))
  (println (typecheck (parse val-fn-double)))
)

(main)