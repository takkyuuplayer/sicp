(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))
(define random-init 137)

; 3.81
(define (random-numbers stream)     ; e.g. stream = ('generate 'generate 123 256 'generate .....)
  (let ((m (stream-car stream)))
    (define (next m x)
      (if (eq? m 'generate) (rand-update x) m)
      )
      (cons-stream random-init (stream-map next stream (random-numbers stream)))
    )
  )

; test 
(define s0 (cons-stream 'generate s0))
(define s1
  (cons-stream
    'generate
    (cons-stream
      'generate (cons-stream 4457 s0))))

(define rs0 (random-numbers s0))
(define rs1 (random-numbers s1))
(stream-head rs0 20)
(stream-head rs1 10)
