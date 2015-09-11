(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val)) serialized-p)))


(define (make-mutex)
  (define (test-and-set! cell)
    (if (car cell)
      true
      (begin (set-car! cell #t)
             #f)))
  (define (clear! cell)
      (set-car! cell #f))
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

; 3.48

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

(define issue-account-id (auto-incrementor))

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
            ((eq? m 'account-id) account-id)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (
        (if (> (account1 'account-id) (account2 'account-id))
            (serializer1 (serializer2 exchange))
            (serializer2 (serializer1 exchange))
        )
     account1
     account2)))


; Test

(define account1 (make-account-and-serializer 100))
(define account2 (make-account-and-serializer 200))
(print (account1 'balance))
(print (account2 'balance))

(serialized-exchange account1 account2)
(print (account1 'balance))
(print (account2 'balance))

