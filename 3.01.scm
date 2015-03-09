(use slib)
(require 'trace)

(define (make-accumulator sum)
  (lambda (amount) (begin (set! sum (+ sum amount)) sum)))

(define A (make-accumulator 5))
(print (A 10))
(print (A 10))
