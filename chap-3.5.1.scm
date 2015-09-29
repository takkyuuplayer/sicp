(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x) (* x x))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (( divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; w/o list
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(print (sum-primes 10000 1000000))

;==================================================

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


; w/ list
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

(print (sum-primes 10000 1000000))
