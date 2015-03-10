(define (last-pair list1)
  (if (null? (cdr list1))
    (car list1)
    (last-pair (cdr list1)
               )))

(print (last-pair (list 23 72 149 34)))
