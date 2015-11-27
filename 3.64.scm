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
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; 3.64
(define (stream-limit s tolerance)
  (let ((a (stream-car s))
         (b (stream-car (stream-cdr s)))
         )
     (if (< (abs (- a b)) tolerance)
       b
       (stream-limit (stream-cdr s) tolerance)
       )
     ))

; test
(define (sqrt-improve guess x)
    (average guess (/ x guess)))
(define (average a b) (/ (+ a b) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
(print (stream-limit (sqrt-stream 2) 1e-1))  ; 1.4166666666666665
(print (stream-limit (sqrt-stream 2) 1e-10)) ; 1.414213562373095
