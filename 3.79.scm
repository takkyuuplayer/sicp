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

(print (stream-ref (solve-2nd 1 2 1 1 0.001) 1000))

; 3.79
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define func ; steam-map apply values (not stream!) to proc
  (lambda (dy y)
    (+ (* dy 1) (* y 2))
    ))
(print (stream-ref (solve-2nd func 1 1 0.001) 1000))
