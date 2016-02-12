(use srfi-27)
(random-source-randomize! default-random-source)
(define (random n)
  (* n (random-real))
  )

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed
                  ))))
  (iter trials 0))

; 3.5

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define area (abs (* (- x1 x2) (- y1 y2))))
  (* area (monte-carlo trials p))
  )
(define (plot-in-circle)
  (let (
        (x (random-in-range -1 1))
        (y (random-in-range -1 1)))
    (<= (+ (* x x) (* y y)) 1)
    ))
(define (estimate-pi trials)
  (estimate-integral plot-in-circle -1 1 -1 1 trials)
  )
(print (estimate-pi 100000.0))
