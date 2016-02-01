(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

; 3.80
(define (RLC R L C dt)
  (define (circuit vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams
                  (scale-stream vc (/ 1 L))
                  (scale-stream il (* R (/ -1 L)))
                  )
      )
    (cons vc il)
    )
  circuit
)

(define test (RLC 1 1 0.2 0.1))
(stream-head (car (test 10 0)) 10)
(stream-head (cdr (test 10 0)) 10)
