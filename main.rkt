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
  (lambda x T t)        abstraction
  (app t t)             application
  (succ t)              successor
  (pred t)              predecessor
  (iszero t)            is zero
  (if t then t else t)  conditional
v ::=                   values:
  true | false          boolean constants
  n                     integer constants
  (lambda x T t)        abstraction value
T(B) ::=                types:
  auto                  infer type
  (B p)                 liquid type
  (T(B) -> T(B))        type of functions; disallow auto for function type
B ::=                   base types:
  Bool                  boolean
  Int                   integer

Predicates
p ::=                   liquid predicates:
  q                     predicate
  k                     liquid type variable
q ::=                   predicates:
  x                     variable
  true | false          boolean constants
  (iop expr expr)       inequalities
  (lop q q)             logic operations
iop ::=                 inequality operations:
  <
  <=
  ==
  >=
  >
  !=
lop ::=                 logic operations:
  and
  or
expr ::=
  (aop expr expr)       arithmetic
  n                     integer numbers
  x                     variable
aop ::=
  +
  -
n   ::=                 integer numbers:
  0, -1, 1, ...

Context
G ::=                   contexts:
  '()                   empty context
  (dict-set G x T)      term variable binding
|#

(define (test)
  (run-tests parse-tests)
  (run-tests typecheck-tests))

(define (main)
  (println (typecheck (parse sltlc-val-true)))
  (println (typecheck (parse sltlc-val-false)))
  (println (typecheck (parse sltlc-val-fn-identity)))
  (println (typecheck (parse sltlc-term-app-identity)))
  (println (typecheck (parse sltlc-val-fn-double)))
  (println (typecheck (parse sltlc-val-22-5-5)))
  (println (typecheck (parse sltlc-val-zero)))
  (println (typecheck (parse sltlc-term-succ)))
  (println (typecheck (parse sltlc-term-pred)))
)

(main)
(test)