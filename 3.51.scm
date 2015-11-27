(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

; 3.51
(define (show x)
    (print x)
      x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(print "-------")
(stream-ref x 7)
(print "-------")
(stream-ref x 5)
