
(define p (cons 1 2))
(set-car! p 2)
(displayln p)

(define (main . args)
  (run-benchmark
    "test"
    5
    (lambda _ void)
    (lambda _ void)
    p
    p))
