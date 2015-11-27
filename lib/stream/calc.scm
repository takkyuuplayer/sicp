(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream) (stream-cdr stream))
               ))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))
