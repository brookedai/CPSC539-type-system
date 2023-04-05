#lang plai

(require racket
         rackunit
         rackunit/text-ui)
(require "stages/sltlc-parser.rkt"
         "stages/sltlc-typechecker.rkt")

#|
SIMPLE LIQUID-TYPED LAMBDA CALCULUS (SLTLC)
Surface syntax
t ::=                   terms:
  x                     variable
  (lambda x T t)        abstraction
  (app t t)             application
  (succ t)              successor
  (pred t)              predecessor
  (ineq iop t t)        inequality
  (binop aop t t)       binary arithmetic
  (if t then t else t)  conditional
v ::=                   values:
  true | false          boolean constants
  n                     integer constants
  (lambda x T t)        abstraction value
aop ::=                 arithmetic operators:
  +
  -
  *
  /

Types
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
  =
  >=
  >
  !=
lop ::=                 logic operations:
  and
  or
expr ::=
  (laop expr expr)      linear arithmetic
  n                     integer numbers
  x                     variable
laop ::=                linear arithmetic operators:
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
  (pretty-print (typecheck (parse sltlc-val-true)))
  (pretty-print (typecheck (parse sltlc-val-false)))
  (pretty-print (typecheck (parse sltlc-val-fn-identity)))
  (pretty-print (typecheck (parse sltlc-term-app-identity)))
  (pretty-print (typecheck (parse sltlc-val-fn-double)))
  (pretty-print (typecheck (parse sltlc-val-22-5-5)))
  (pretty-print (typecheck (parse sltlc-val-zero)))
  (pretty-print (typecheck (parse sltlc-term-succ)))
  (pretty-print (typecheck (parse sltlc-term-pred)))
  (pretty-print (typecheck (parse sltlc-term-ineq)))
  (pretty-print (typecheck (parse sltlc-term-binop)))
  (pretty-print (typecheck (parse sltlc-term-if)))
  (pretty-print (typecheck (parse sltlc-val-div-0)))
  (pretty-print (typecheck (parse sltlc-term-div-0-error)))
)

(main)
(test)