(use slib)
(require 'trace)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)    ; 記号
                             (cadr pair))  ; 頻度
                  (make-leaf-set (cdr pairs))))))

(define pairs (list '(A 4) '(B 2) '(C 1) '(D 1)))
;(print pairs)

(define leaf-set (make-leaf-set pairs))
;(print leaf-set)
;(print (make-code-tree (car leaf-set) (cadr leaf-set)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
(print sample-tree)

; 2.69


(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
    (car leaf-set)
    (let ((left (car leaf-set))
          (right (cadr leaf-set))
          (rest (cddr leaf-set)))
      (successive-merge (adjoin-set (make-code-tree left right) rest))
      )))

;(trace successive-merge)
(print (successive-merge leaf-set))
