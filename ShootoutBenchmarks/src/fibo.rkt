#lang racket/base
(require "time-run.rktl")

(define (fib n)
  (cond ((< n 2) 1)
        (else (+ (fib (- n 2)) (fib (- n 1))))))

(time-run fib)