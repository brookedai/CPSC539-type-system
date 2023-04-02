#lang racket

(require racket 
         racket/struct)
(provide (all-defined-out))

#|
  STLTC TYPES

  Implementation notes:
  #:transparent - so that struct equality works.
  #:methods gen:custom-write - so that struct prints nicely.
|#

(struct type ()
  #:transparent)
(struct bool type ()
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'type:bool)
        (lambda (obj) (list))))])
(struct fun type (param body)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'type:fun)
        (lambda (obj) (list (fun-param obj) (fun-body obj)))))])
(struct type-var type (name)
  #:transparent
  #:methods gen:custom-write
     [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'type:type-var)
        (lambda (obj) (list (type-var-name obj)))))])

;; Template
; (define (fn-for-type t)
;   (cond [(bool? t) (... t)]
;         [(fun? t)  (... (fn-for-type (fun-param t))
;                         (fn-for-type (fun-body t)))]
;         [(type-var? t) (... (type-var-name t))])

;; Helpers
(define (primitive-val? v)
  (boolean? v))
