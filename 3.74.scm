(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (sign-change-detector after before)
  (define (>= a b) (or (> a b) (= a b)))
  (cond ((and (>= before 0) (>= after 0)) 0)
        ((and (>= before 0) (< after 0)) -1)
        ((and (< before 0) (>= after 0) 1))
        (else 0)
        )
  )

(define sense-data
    (stream-map (lambda (x) (sin x)) integers))
(define (make-zero-crossings input-stream last-value)
    (cons-stream
         (sign-change-detector (stream-car input-stream) last-value)
            (make-zero-crossings (stream-cdr input-stream)
                                                         (stream-car input-stream))))
(define zero-crossings (make-zero-crossings sense-data 0))

(stream-head zero-crossings 10) ; 0, 0, 0, -1, 0, 0, 1, 0, 0, -1,

; 3.74

(define zero-crossings
    (stream-map
      sign-change-detector
      sense-data
      (cons-stream 0 sense-data)
                ))
(stream-head zero-crossings 10)
