(define (display-stream s)
  (stream-for-each print s))

(define (stream-head s n)
  (define (iter s n)
    (if (<= n 0)
      (print "")
      (begin
        (display (stream-car s))
        (display ", ")
        (iter (stream-cdr s) (- n 1)))))
  (iter s n))

