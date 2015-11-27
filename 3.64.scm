(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")

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
