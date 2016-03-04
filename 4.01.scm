(define expression '((print "left") (print "right")))


; gauche default

(print expression)

; 4.01 left to right
(define (list-of-values-left-to-right exps)
  (if (null? exps)
    '()
    (let ((first-eval (eval (car exps) (interaction-environment))))
      (cons first-eval
            (list-of-values-left-to-right (cdr exps))))))

(list-of-values-left-to-right expression)

; 4.01 right to left
(define (list-of-values-right-to-left exps)
  (if (null? exps)
    '()
    (let ((first-eval (list-of-values-right-to-left (cdr exps))))
      (cons (eval (car exps) (interaction-environment))
            first-eval))))

(list-of-values-right-to-left expression)

