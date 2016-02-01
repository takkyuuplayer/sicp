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

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

; 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))        ; y   = integral dy
  (define dy (integral (delay ddy) dy0 dt))     ; dy  = integral ddy
  (define ddy (add-streams                      ; ddy = (a * dy) + ( b * y)
                (scale-stream dy a)
                (scale-stream y b))
    )
  y
  )

; (a, b, y0, dy0) = (1, 0, 1, 1) will be the same as 3.77 equation
(print (stream-ref (solve-2nd 1 0 1 1 0.001) 1000))
