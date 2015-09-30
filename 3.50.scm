(define stream-null? null?)
(define the-empty-stream '())
(define-syntax cons-stream
  (syntax-rules ()
                ((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; test
(map print
    (stream-map +
                (stream-enumerate-interval 1 10)
                (stream-enumerate-interval 11 20)
    )
    )
