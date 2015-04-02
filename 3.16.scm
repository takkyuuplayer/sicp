(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

; 3.16
(define count-3
  (list 1 2 3))
(print (count-pairs count-3))

(define x (cons 1 2))
(define count-4 (list x x))
(print (count-pairs count-4))

(define y (cons x x))
(define count-7 (cons y y))
(print (count-pairs count-7))
