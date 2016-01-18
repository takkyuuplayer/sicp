(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; 3.73
(define (rc R C dt)
  (define (circuit i v0)
    (add-streams
      (scale-stream (integral i v0 dt) (/ 1 C))
      (scale-stream i R)
      )
    )
  circuit
  )

(stream-head ((rc 5 1 0.5) integers 1) 10)
