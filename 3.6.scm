(use slib)
(require 'trace)

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))
(define random-init 137)

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (lambda () (set! x (rand-update x)) x))
            ((eq? m 'reset)
             (lambda (seed) (set! x seed)))
            (else (error "Unknown request"))
            )
      )
    dispatch
    ))

(print ((rand 'generate)))
(print ((rand 'generate)))
(print ((rand 'generate)))
(print ((rand 'generate)))
(print ((rand 'generate)))
(print "Reset to 9182")
(print ((rand 'reset) 9182))
(print ((rand 'generate)))
(print ((rand 'generate)))
