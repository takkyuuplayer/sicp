(define stream-null? null?)
(define the-empty-stream '())
( define (memo-proc proc)
         (let ((already-run? #f) (result #f))
           (lambda ()
             (if (not already-run?)
               (begin (set! result (proc))
                      (set! already-run? #t)
                      result)
               result))))
(define-syntax cons-stream
  (syntax-rules ()
                ((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))


(define (stream-map proc . argstreams)
  (if (stream-null? (stream-car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))
(define (display-line x)
    (newline)
      (display x))

(define (add-streams s1 s2)
    (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

; merge test
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
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

; 3.56
