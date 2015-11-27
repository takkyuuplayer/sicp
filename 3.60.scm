(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


; 3.59
(define (integrate-stream series-stream)
  (stream-map / series-stream integers)
  )
(define integrated-all-one-series (integrate-stream ones))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define minus-ones (cons-stream -1 minus-ones))
(define cosine-series (cons-stream 1 (mul-streams minus-ones (integrate-stream sine-series))))
(define sine-series (cons-stream 0 (integrate-stream cosine-series)))
(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) ; (定数項)
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1)) ; (s1 定数項) * (s2 の1次以降 ; s1 定数項× s2定数項は↑で出力済みなので無くす)
                 (mul-series s2 (stream-cdr s1))                ; (s1 の1次以降) * (s2)
                 )
               ))
(define one (add-streams (mul-series sine-series sine-series)
                         (mul-series cosine-series cosine-series)))
(stream-head one 5)

