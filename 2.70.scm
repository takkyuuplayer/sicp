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

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
    (car leaf-set)
    (let ((left (car leaf-set))
          (right (cadr leaf-set))
          (rest (cddr leaf-set)))
      (successive-merge (adjoin-set (make-code-tree left right) rest))
      )))

(define (in? s l)
  (cond ((null? l) #f)
        ((equal? s (car l)) #t)
        (else (in? s (cdr l)))))

(define (encode-symbol symbol tree)
  (cond
    ((and (in? symbol (symbols tree)) (leaf? tree)) ())
    ((in? symbol (symbols (left-branch tree)))
     (cons 0 (encode-symbol symbol (left-branch tree))))
    ((in? symbol (symbols (right-branch tree)))
     (cons 1 (encode-symbol symbol (right-branch tree))))
    (else (error "bad letter -- CHOOSE-BRANCH" symbol))))


(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; 2.70

(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(print (make-leaf-set pairs))
(define huffman (successive-merge (make-leaf-set pairs)))
(print huffman)

(define encoded (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) huffman))

(print encoded)

; raw = 3 [bit/word] * 36 [word] = 108 bit
; encoded = 84 bit
