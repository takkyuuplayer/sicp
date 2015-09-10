(define (auto-incrementor)
  (let ((id 0))
    (lambda ()
      (let ((new-id (+ id 1))) ; TODO: serialize incremention
        (set! id new-id)
        id
        )
      )
    )
  )

(define issue-account-id (incrementor))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define account-id (issue-account-id))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'account-id) (lambda () account-id))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (> (account1 'account-id) (account2 'account-id))
      (serializer1 (serializer2 exchange))
      (serializer2 (serializer1 exchange))
     account1
     account2)))

