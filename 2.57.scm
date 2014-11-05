(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
    (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
    (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation a1 a2)
  (cond ((and (number? a1) (number? a2)) (expt a1 a2))
        ((=number? a2 1) a1)
        ((=number? a2 0) 1)
        (else (list '** a1 a2))
        ))

(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
            (make-exponentiation (base exp)
                                 (make-sum (exponent exp) -1)
                                 )
         ; oh,,,
            (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))

; 2.57

(define (addend s) (cadr s))
(define (augend s)
  (let ((second (caddr s))
        (rest (cdddr s))
        )
    (if (null? rest)
      second
      (cons '+ (cons second rest)))))

(print (augend '(+ a (+ b c d))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((second (caddr p))
        (rest (cdddr p))
        )
    (if (null? rest)
      second
      (cons '* (cons second rest)))))

(print (multiplicand '(* a b c d)))
(print (deriv '(* x y (+ x 3)) 'x))

; tsuchi
(define (exp-cdr p)
  (append (list (car p)) (cddr p)))

(define (augend p)
  (if (null? (cdddr p))
    (caddr p)
    (exp-cdr p)))
(define (multiplicand p)
  (if (null? (cdddr p))
    (cadr p)
    (exp-cdr p)))

(print (augend '(+ a b)))
(print (augend '(+ a b c d)))
(print (deriv '(* x y (+ x 3)) 'x))
