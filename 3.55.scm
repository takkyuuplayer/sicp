(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")

(define (add-streams s1 s2)
    (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


; 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream) (stream-cdr stream))
               ))

(define t (partial-sums integers))
(print (stream-ref t 0))
(print (stream-ref t 1))
(print (stream-ref t 2))
(print (stream-ref t 3))
(print (stream-ref t 4))
