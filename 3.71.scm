(load "./lib/stream/base.scm")
(load "./lib/stream/list.scm")
(load "./lib/stream/calc.scm")
(load "./lib/stream/display.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (merge-weighted pairs1 pairs2 weight)
  (cond ((stream-null? (stream-car pairs1)) pairs2)
        ((stream-null? (stream-car pairs2)) pairs1)
        (else
          (let ((p1car (stream-car pairs1))
                (p2car (stream-car pairs2)))
            (if (< (weight p1car) (weight p2car))
              (cons-stream p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
              (cons-stream p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

; 3.71
(define (weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i) (* j j j))))
(define ramujan-search-pairs
  (weighted-pairs integers integers weight)
  )
(define (ramujan-filter pairs)
  (let ((pair1 (stream-car pairs))
        (pair2 (stream-car (stream-cdr pairs))))
    (if (= (weight pair1) (weight pair2))
      (cons-stream (weight pair1) (ramujan-filter (stream-cdr pairs)))
      (ramujan-filter (stream-cdr pairs)))))

(stream-head (ramujan-filter ramujan-search-pairs) 10)
