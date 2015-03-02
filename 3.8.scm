(define (create-f)
  ((lambda ()
    (define ans 1)
    (lambda (x)
      (set! ans (* ans x))
      ))
  ))

; from left
(define f (create-f))
(print (f 0))
(print (f 1))

; from right
(define f (create-f))
(print (f 1))
(print (f 0))
