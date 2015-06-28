* or-gate

```scm
(define (or-gate i1 i2 o)
  ((let ((c1 (make-wire))
         (c2 (make-wire))
         (c3 (make-wire))
         )
     (inverter i1 c1)
     (inverter i2 c2)
     (and-gate c1 c2 c3)
     (inverter c3 o)
     )))
```

* delay

```
2 * (inverter-delay) + (and-gate-delay)
```
