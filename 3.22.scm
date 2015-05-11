(define (make-queue)
  (let ((front-ptr ())
        (rear-ptr ()))

    (define (empty-queue?)
      (null? front-ptr)
      )
    (define (front-queue)
      (if (empty-queue?)
        (error "EMPTY!!")
        (car front-ptr)
        )
      )
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (car front-ptr))
              (else
                (set! rear-ptr new-pair)
                (set! front-ptr (append front-ptr new-pair))
                front-ptr
                )
              )
        )
      )

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
              (set! front-ptr (cdr front-ptr))
              front-ptr)))

    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'front-queue) front-queue)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        )
      )
    dispatch
    )
  )

(define q1 (make-queue))
(print ((q1 'insert-queue!) '1))
(print ((q1 'insert-queue!) '2))
(print ((q1 'insert-queue!) '3))
(print ((q1 'insert-queue!) '4))
(print ((q1 'delete-queue!)))
(print ((q1 'delete-queue!)))
(print ((q1 'delete-queue!)))
(print ((q1 'delete-queue!)))
(print ((q1 'empty-queue?)))
(print ((q1 'front-queue)))
