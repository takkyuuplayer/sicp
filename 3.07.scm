(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda (args...) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'identify) (eq? p password))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; 3.7
(define (make-joint account orig-pass password)
  (if (account orig-pass 'identify)
    (lambda (p m)
      (cond ((not (eq? p password)) (lambda (args...) "Incorrect password"))
            (else (account orig-pass m))
            ))
    (print "Invalid password")
    ))


(define acc (make-account 100 'secret-password))
(print ((acc 'secret-password 'withdraw) 10))

(print 'copy-acc1)
(define copy-acc (make-joint acc 'secret-password 'my-password))
(print ((copy-acc 'secret-password 'withdraw) 10))
(print ((copy-acc 'my-password 'withdraw) 10))

(print 'copy-acc2)
(define copy-acc2 (make-joint copy-acc 'my-password 'foobar))
(print ((copy-acc2 'foobar 'withdraw) 10))

(print 'invalid)
(define invalid-acc (make-joint acc 'invalid-password 'my-password))
(print ((invalid-acc 'my-password 'withdraw) 10))
