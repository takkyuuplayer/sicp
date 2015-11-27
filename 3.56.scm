(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")

; 3.56
(define S (cons-stream 1 (merge
                           (scale-stream S 2)
                           (merge (scale-stream S 3) (scale-stream S 5))
                           )))
(print (stream-ref S 0))
(print (stream-ref S 1))
(print (stream-ref S 2))
(print (stream-ref S 3))
(print (stream-ref S 4))
(print (stream-ref S 5))
(print (stream-ref S 6))
(print (stream-ref S 7))
(print (stream-ref S 8))
(print (stream-ref S 9))
(print (stream-ref S 10))

