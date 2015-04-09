(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; 3.17
(define (count-pairs pair)
  (define walked-pairs (list))
  (define (walk-pair pair)
    (if (and (pair? pair)
             (not (element-of-set? pair walked-pairs)))
      (begin (set! walked-pairs (append walked-pairs (list pair)))
             (walk-pair (car pair))
             (walk-pair (cdr pair))
             )
      ()
      ))
  (walk-pair pair)
  (length walked-pairs)
  )


; from 3.16
(define count-3
  (list 1 2 3))
(print (count-pairs count-3))

(define x (cons 1 2))
(define count-4 (list x x))
(print (count-pairs count-4))

(define y (cons x x))
(define count-7 (cons y y))
(print (count-pairs count-7))
