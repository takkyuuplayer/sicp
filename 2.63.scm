(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2.63

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define left-tree (make-tree 7
                             (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                             (make-tree 9 '() (make-tree 11 '() '()))
                             ))
(define middle-tree (make-tree 7
                               (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                               (make-tree 9 '() (make-tree 11 '() '())
                                          )))
(define right-tree (make-tree 5
                              (make-tree 3 (make-tree 1 '() '()) '())
                              (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))
                              ))

(print (tree->list-1 left-tree))
(print (tree->list-1 middle-tree))
(print (tree->list-1 right-tree))

; 2.63 a.
Same result

; 2.63 b.
