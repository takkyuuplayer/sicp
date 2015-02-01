(use slib)
(require 'trace)

(define (make-monitored f)
  (let ((mf 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) mf)
            ((eq? x 'reset-count) (set! mf 0))
            (else (begin (set! mf (+ mf 1))
               (f x))
        )))))

(define s (make-monitored sqrt))

(print (s 100))
(print (s 36))
(print (s 'how-many-calls?))
(print (s 'reset-count))
(print (s 'how-many-calls?))
