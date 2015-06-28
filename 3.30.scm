; 3.30
(define (ripple-carry-adder list-a list-b list-s c-out)
  (let ((a (car list-a))
        (b (car list-b))
        (sum (car list-s))
        (c-in (make-wire))
        )
    (if (null? a)
      (set-signal! c-in 0)
      (ripple-carry-adder (cdr list-a) (cdr list-b) (cdr list-s) c-in)
      )
    (full-adder a b c-in sum c-out)
    ))

; delay
; half-adder-delay = max of ( inverter-delay + 2 * and-gate-delay, or-gate-delay + and-gate-delaoy)
; full-adder-delay = 2 * half-adder-delay + or-gate-delay
; total-delay = n * full-adder-delay
