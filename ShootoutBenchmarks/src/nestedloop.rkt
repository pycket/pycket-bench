#lang racket/base
(require "time-run.rktl" (for-syntax racket/base))

(require mzlib/defmacro)

(define-macro (nest n expr)
  (if (> n 0)
      `(let loop ([i 1]) (unless (> i n)
                           (nest ,(- n 1) ,expr)
                           (loop (add1 i))))
      expr))

(define (bench n)
  (let* ([x 0])
    (nest 6 (set! x (+ x 1)))
    (printf "~s\n" x)))

(time-run bench)
