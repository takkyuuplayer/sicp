; eval
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

; and
(define (and? exp)
  (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (expand-and exp)
    (let ((first (my-eval (car exp) env)))
      (if (not first)
        #f
        (if (null? (cdr exp))
          first
          (expand-and (cdr exp))))))
  (if (null? (cdr exp))
    (my-eval #t env)
    (expand-and (cdr exp)))
  )

; or
(define (or? exp)
  (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (expand-or exp)
    (let ((first (my-eval (car exp) env)))
      (if first
        #t
        (if (null? (cdr exp))
          first
          (expand-or (cdr exp))))))
  (if (null? (cdr exp))
    (my-eval #f env)
    (expand-or (cdr exp)))
  )

(print (my-eval '(and) (interaction-environment)))
(print (my-eval '(and #t #t) (interaction-environment)))
(print (my-eval '(and #t #f) (interaction-environment)))
(print (my-eval '(or) (interaction-environment)))
(print (my-eval '(or #t #t) (interaction-environment)))
(print (my-eval '(or #t #f) (interaction-environment)))
