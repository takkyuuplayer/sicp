(use srfi-27)
(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(random-source-randomize! default-random-source)
(define (random n)
  (* n (random-real))
  )
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; 3.82
(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo-stream (stream-cdr experiment-stream) passed failed)
                 )
    )
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))
    )
)

(define (estimate-integral-stream p-stream x1 x2 y1 y2)
  (define area (abs (* (- x1 x2) (- y1 y2))))
  (scale-stream (monte-carlo-stream p-stream 0 0) area)
  )

; test
(define (plot-in-circle)
  (let (
        (x (random-in-range -1 1))
        (y (random-in-range -1 1)))
    (<= (+ (* x x) (* y y)) 1)
    ))

(define (plot-in-circle-stream) (cons-stream (plot-in-circle) (plot-in-circle-stream)))
(define estimate-pi-stream (estimate-integral-stream (plot-in-circle-stream) -1 1 -1 1))
(print (stream-ref estimate-pi-stream 10.0))
(print (stream-ref estimate-pi-stream 100.0))
(print (stream-ref estimate-pi-stream 1000.0))
(print (stream-ref estimate-pi-stream 10000.0))
