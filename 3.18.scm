(define (has-loop pair)
  (define (find-loop pair walked)
    (if (memq pair walked)
      #t
      (if (pair? pair)
        (or
          (find-loop (car pair) (append walked (list pair)))
          (find-loop (cdr pair) (append walked (list pair)))
          )
        #f
        )
      )
    )
  (find-loop pair (list))
  )

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x (list 1 2))
(print (has-loop x))

(define looped (make-cycle x))
(print (has-loop looped))
