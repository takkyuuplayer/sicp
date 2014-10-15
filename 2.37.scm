(define mat
  (list (list 1 2 3)
        (list 4 5 6)
        (list 4 8 9)
        )
  )
(define vec
  (list 1 1 1))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (v1) (dot-product v1 v)) m))

(print (matrix-*-vector mat vec))

