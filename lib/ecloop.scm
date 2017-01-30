(load "./lib/register-machine.scm")
(load "./lib/operations.scm")
(load "./lib/eceval.scm")

(define the-global-environment (setup-environment))
(start eceval)
