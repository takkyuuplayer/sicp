(use slib)
(require 'trace)

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

; list
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; display
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (stream-head s n)
  (define (iter s n)
    (if (<= n 0)
      (print "end;")
      (begin
        (display (stream-car s))
        (display ", ")
        (iter (stream-cdr s) (- n 1)))))
  (iter s n))

; calc
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


; 3.59
(define (integrate-stream series-stream)
  (stream-map / series-stream integers)
  )
(define integrated-all-one-series (integrate-stream ones))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define minus-ones (cons-stream -1 minus-ones))
(define cosine-series (cons-stream 1 (mul-streams minus-ones (integrate-stream sine-series))))
(define sine-series (cons-stream 0 (integrate-stream cosine-series)))
(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) ; (定数項)
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1)) ; (s1 定数項) * (s2)
                 (mul-series s2 (stream-cdr s1))                ; (s1 の1次以降) * (s2)
                 )
               ))
(define one (add-streams (mul-series sine-series sine-series)
                         (mul-series cosine-series cosine-series)))
(stream-head one 5)

