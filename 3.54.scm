(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")

(define ones (cons-stream 1 ones))
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))


; 3.54 mul-streams
(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))
(print (stream-ref factorials 0))
(print (stream-ref factorials 1))
(print (stream-ref factorials 2))
(print (stream-ref factorials 3))
(print (stream-ref factorials 4))
