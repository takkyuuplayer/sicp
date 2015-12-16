(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

; 3.69
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x)) (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
      )
    ))

(stream-head (triples integers integers integers) 10)

(define pythagoras (stream-filter
                     (lambda (x) (let ((a (car   x))
                                        (b (cadr  x))
                                        (c (caddr x)))
                                    (= (+ (* a a) (* b b)) (* c c))
                                    ))
                     (triples integers integers integers)
                     )
 )
(stream-head pythagoras 4)

