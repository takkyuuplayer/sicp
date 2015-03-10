(define (>= a b) (or (> a b) (= a b)))

(define (func a b c)
  (cond ((and (>= a c) (>= b c)) (+ (* a a) (* b b)))
        ((and (>= b a) (>= c a)) (+ (* b b) (* c c)))
        (else (+ (* c c) (* a a)))
        )
  )
