(define a 1)
(print (number? a))
(print (number? 1))

(print (number? 1))

(print (symbol? 'x))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(print (same-variable? 'v1 'v1))
(print (same-variable? 'v1 'v2))

(define (make-sum a1 a2) (list '+ a1 a2))
(print (make-sum 'a1 'a2))

(define (make-product m1 m2) (list '* m1 m2))
(print (make-product 'a1 'a2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(print (sum? (make-sum 'a1 'a2)))

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

(print (deriv '(+ x 3) 'x))
(print (deriv '(* x y) 'x))
(print (deriv '(* (* x y) (+ x 3)) 'x))

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

(print (deriv '(+ x 3) 'x))
(print (deriv '(* x y) 'x))
(print (deriv '(* (* x y) (+ x 3)) 'x))

(print (expt 5 2))

; 2.56
(define (make-exponentiation a1 a2)
  (cond ((and (number? a1) (number? a2)) (expt a1 a2))
        ((=number? a2 1) a1)
        ((=number? a2 0) 1)
        (else (list '** a1 a2))
        ))
(print (make-exponentiation 2 2))
(print (make-exponentiation 2 1))
(print (make-exponentiation 2 0))
(print (make-exponentiation 'a 2))

(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(print (base (make-exponentiation 'a 2)))
(print (exponent (make-exponentiation 'a 2)))
(print (exponentiation? (make-exponentiation 'a 2)))

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

(print (deriv (make-exponentiation 'x 5) 'x))
(print (deriv (make-exponentiation 'x 'b) 'x))
