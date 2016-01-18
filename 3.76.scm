(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (sign-change-detector after before)
  (cond ((and (>= before 0) (>= after 0)) 0)
        ((and (>= before 0) (< after 0)) -1)
        ((and (< before 0) (>= after 0) 1))
        (else 0)
        )
  )

(define sense-data
  (stream-map (lambda (x) (sin x)) integers))

; 3.76
(define (smooth stream)
  (cons-stream
    (/ (+ (stream-car stream) (stream-car (stream-cdr stream))) 2)
    (smooth (stream-cdr stream)))
  )

(define zero-crossings
    (stream-map
      sign-change-detector
      (smooth sense-data)
      (cons-stream 0 (smooth sense-data))
                ))
(stream-head zero-crossings 10)
