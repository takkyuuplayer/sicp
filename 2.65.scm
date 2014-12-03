(use slib)
(require 'trace)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else
         (lookup given-key (right-branch set-of-records)))))

(define (key set) (car set))

(define tree1
  (list '(3 10)
        (list '(2 20) (list '(1 10) () ()) ())
        (list '(4 40) () (list '(5 50) ()))
        ))

(print (lookup 0 tree1)) ; #f
(print (lookup 1 tree1)) ; (1 10)
(print (lookup 2 tree1)) ; (2 20)
(print (lookup 3 tree1)) ; (3 10)
(print (lookup 4 tree1)) ; (4 40)
(print (lookup 5 tree1)) ; (5 50)
