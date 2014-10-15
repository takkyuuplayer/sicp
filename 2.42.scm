(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(print (enumerate-interval 1 5))

(define (queens board-size)
  (define (queen-cols k) ; 1
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions)) ; 1
        (flatmap ; array of board
          (lambda (rest-of-queens)
            ; (print rest-of-queens)
            (map (lambda (new-row) ; ((1 2) (2 4) (3 1) (4 3))
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position row col rest)
  (append rest (list (list row col))))

(define (getK k l)
    (if (< k 2) (car l) (getK (- k 1) (cdr l))))
(define (getUntilK k l)
    (if (> k 1) (cons (car l) (getUntilK (- k 1) (cdr l))) ()))


(print (adjoin-position 1 2 (list (list 0 1) (list 2 4) (list 3 1))))
  (define (non-safe? position1 position2)
    (let ((row1 (car position1))
          (col1 (cadr position1))
          (row2 (car position2))
          (col2 (cadr position2)))
      (or (= row1 row2)
          (= (- row2 row1) (- col2 col1))
          (= (- row2 row1) (- col1 col2))
        )
      ))
(define (safe? k positions)
  (let
    ((last (getK k positions)) (rest (getUntilK k positions)))
    (= 0 (length (filter (lambda (position) (non-safe? last position)) rest)))
    ))

(define sample-roq (list (list 2 1) (list 4 2) (list 1 3)))
(define p1 (adjoin-position 1 4 sample-roq))
(define p2 (adjoin-position 2 4 sample-roq))
(define p3 (adjoin-position 3 4 sample-roq))
(define p4 (adjoin-position 4 4 sample-roq))

(print (safe? 4 p1))
(print (safe? 4 p2))
(print (safe? 4 p3))
(print (safe? 4 p4))

(define empty-board ())
(print (queens 4))
(print (length (queens 7)))
