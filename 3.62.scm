(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1))
                 (mul-series s2 (stream-cdr s1))
                 )
               ))
(define (invert-unit-series s1)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr s1) -1)
                             (invert-unit-series s1)
                             )))
(define (integrate-stream series-stream)
  (stream-map / series-stream integers)
  )
(define minus-ones (cons-stream -1 minus-ones))
(define cosine-series (cons-stream 1 (mul-streams minus-ones (integrate-stream sine-series))))
(define sine-series (cons-stream 0 (integrate-stream cosine-series)))


; 3.62
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define cosine-series (cons-stream 1 (mul-streams minus-ones (integrate-stream sine-series))))
(define sine-series (cons-stream 0 (integrate-stream cosine-series)))
(define tangent-series (div-series sine-series cosine-series))

(stream-head tangent-series 10)
