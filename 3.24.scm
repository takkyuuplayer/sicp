(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; test
(define origin-table
  (make-table equal?)
  )
((origin-table 'insert-proc!) 'row1 'col1 'val1-1)
((origin-table 'insert-proc!) 'row1 'col2 'val1-2)
(print ((origin-table 'lookup-proc) 'row1 'col1))
(print ((origin-table 'lookup-proc) 'row1 'col2))

(define mod3-table
  (make-table
    (lambda (lookup-key key) (= (remainder lookup-key 3) (remainder key 3)))
    )
  )
((mod3-table 'insert-proc!) 0 0 'val0-0)
((mod3-table 'insert-proc!) 0 1 'val0-1)
(print ((mod3-table 'lookup-proc) 0 0))
(print ((mod3-table 'lookup-proc) 0 3))

