
(define p (cons 1 2))
(set-car! p 2)
(car p)

(define (main . args)
  (run-benchmark
    "test"
    5
    (lambda _ #t)
    (lambda _ #t)
    p
    p))
