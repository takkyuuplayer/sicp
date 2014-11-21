(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; 2.61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< (car set) x)
         (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))

(print (adjoin-set 20 (list 10 100 1000)))
(print (adjoin-set 20 ()))
(print (adjoin-set 100 (list 10 100 1000)))
(print (adjoin-set 10000 (list 10 100 1000)))
(print (adjoin-set 20 ()))

; (car set) < x < (cadr set)
