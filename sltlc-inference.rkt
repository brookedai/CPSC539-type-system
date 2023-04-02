#lang racket

(require racket
         racket/struct)
(provide (all-defined-out))

#|
  HINDLEY-MILNER CONSTRAINTS

  Implementation notes:
  #:transparent - so that struct equality works.
  #:methods gen:custom-write - so that struct prints nicely.
|#

;; S, T are types
(struct constraint (S T)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'constraint)
        (lambda (obj) (list (constraint-S obj) (constraint-T obj)))))])

