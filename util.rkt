#lang racket

(require racket)
(provide (all-defined-out))

(define (unique? lst)
  (not (check-duplicates lst)))

(define type-var-counter 0)
(define (fresh-type-var base-name)
  (set! type-var-counter (+ type-var-counter 1))
  (string->symbol (format "~a~a" base-name type-var-counter)))
  