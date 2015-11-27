(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


; 3.59 a
(define (integrate-stream series-stream)
  (stream-map / series-stream integers)
  )

; 3.59 a test
(define integrated-all-one-series (integrate-stream ones))
(stream-head integrated-all-one-series 5) ; 1, 1/2, 1/3, 1/4

; 3.59 b
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define minus-ones (cons-stream -1 minus-ones))
(define cosine-series (cons-stream 1 (mul-streams minus-ones (integrate-stream sine-series))))
(define sine-series (cons-stream 0 (integrate-stream cosine-series)))

(stream-head sine-series 5)
(stream-head cosine-series 5)
