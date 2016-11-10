(load "./lib/register.scm")

; http://sicp.iijlab.net/fulltext/x524.html
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

; http://www.serendip.ws/archives/3278
(define fact-machine
  (make-machine
    '(continue val n)
    (list (list '= =) (list '- -) (list '* *))
    '(start
       (assign continue (label fact-done))
  fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
  after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  fact-done)))

(define (fact n)
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (format #t "fact:~2d => ~8d" n (get-register-contents fact-machine 'val))
  ((fact-machine 'stack) 'print-statistics)
  (newline))

(define (fact-iter n)
  (if (< n 10)
      (begin
        (fact n)
        (fact-iter (+ n 1)))))

; 5.14
(print (fact 1)) ; (total-pushes = 0 maximum-depth = 0)
(print (fact 2)) ; (total-pushes = 2 maximum-depth = 2)
(print (fact 3)) ; (total-pushes = 4 maximum-depth = 4)
(print (fact 4)) ; (total-pushes = 6 maximum-depth = 6)
(print (fact 5)) ; (total-pushes = 8 maximum-depth = 8)
