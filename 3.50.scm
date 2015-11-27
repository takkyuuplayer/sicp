(load "./lib/stream/base.scm")

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; test
(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define each-sum-stream
    (stream-map +
                (stream-enumerate-interval 1 10)
                (stream-enumerate-interval 11 20)
    ))
(print (stream-ref each-sum-stream 0))
(print (stream-ref each-sum-stream 5))
(print (stream-ref each-sum-stream 9))
(print (stream-ref each-sum-stream 0))
