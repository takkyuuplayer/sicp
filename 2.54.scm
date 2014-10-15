(define (my-equal? list1 list2)

  (cond ((and (null? list1) (null? list2)) #t)
        ((or (null? list1) (null? list2)) #f)
        ((not (eq? (car list1) (car list2))) #f)
        (else (my-equal? (cdr list1) (cdr list2)))
        ))

(print (my-equal? () ()))
(print (my-equal? '(this is a list) '(this is a list)))
(print (my-equal? '(this is a list) '(this (is a) list)))
(print (my-equal? '(this (is a) list) '(this (is a) list)))
;(print (my-equal? '(this is a list) ()))
;

(define (my-equal? list1 list2)

  (cond ((and (null? list1) (null? list2)) #t)
        ((or (null? list1) (null? list2)) #f)
        ((not (eq? (car list1) (car list2))) #f)
        (else (my-equal? (cdr list1) (cdr list2)))
        ))
