(use slib)
(require 'trace)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; 2.2

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end   (end-segment segment)))
    (make-point
      (/ (+ (x-point start) (x-point end)) 2)
      (/ (+ (y-point start) (y-point end)) 2)
      )))

(define p1 (make-point 2 1))
(define p2 (make-point 3 5))
(define seg (make-segment p1 p2))
(print-point (midpoint-segment seg))
