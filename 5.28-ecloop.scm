; q5.28 末尾再帰しない評価器
(load "./lib/register-machine.scm")
(load "./lib/operations.scm")
(load "./5.28-eceval.scm")

(define the-global-environment (setup-environment))
(start eceval)

