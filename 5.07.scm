(load "./lib/register.scm")

; 再帰的
(define expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '* *) (list '- -))
    '((assign continue (label expt-done))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))
  after-expt
    (restore n)
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)))

(print (set-register-contents! expt-machine 'b 2)) ; done
(print (set-register-contents! expt-machine 'n 8)) ; done
(print (start expt-machine)) ; done
(print (get-register-contents expt-machine 'val)) ; 256

; 反復的
(define expt-machine
  (make-machine
    '(b n counter product)
    (list (list '= =) (list '* *) (list '- -))
    '((assign counter (reg n))
     (assign product (const 1))
  expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-loop))
  expt-done)))

(print (set-register-contents! expt-machine 'b 2)) ; done
(print (set-register-contents! expt-machine 'n 8)) ; done
(print (start expt-machine)) ; done
(print (get-register-contents expt-machine 'product)) ; 256
