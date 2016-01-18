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

; 3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
(stream-head (make-zero-crossings sense-data 0 0) 10)

; last-value に avpt を渡してはいけない. 新しい信号の平均値は直前の"実測値"との平均を取らなければならない。
; 最後の実測値 `last-value` と (sign-change-detector) に渡すようの直前の平均値を渡す必要がある。　
