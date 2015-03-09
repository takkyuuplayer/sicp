; ∃ 構成子 `cons` と 選択子`car`, `cdr` s.t. 非負の整数x, yに対し`(car (cons x y)) = x`, `(cdr (cons x y)) = y`とできることを示せば良い

```scm
(define (my-cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (my-car z)
  (if (= (remainder z 2) 0)
    (+ 1 (my-car (/ z 2)))
    0
    ))
(define (my-cdr z)
  (if (= (remainder z 3) 0)
    (+ 1 (my-cdr (/ z 3)))
    0))

(print (my-cons 2 5))
(print (my-car (my-cons 2 5)))
(print (my-cdr (my-cons 2 5)))
