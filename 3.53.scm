(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

; 3.53
(define s (cons-stream 1 (add-streams s s)))
(print (stream-ref s 0)) ; 1
(print (stream-ref s 1)) ; 2
(print (stream-ref s 2)) ; 4
(print (stream-ref s 3)) ; 8
