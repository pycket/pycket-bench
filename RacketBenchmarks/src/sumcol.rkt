#lang racket/base
(require "time-run.rktl")
(require racket/port)

(define (benchargs args)
  (let* ([file-name (if (= (vector-length args) 0)
                        (error "No file to process")
                        (vector-ref args 0))]
         [file      (open-input-file file-name)]
         [out       (open-output-string)])
     (copy-port file out)
     (open-input-string (get-output-string out))))

(define (bench inport)
  (for/fold ([acc 0])
      ([n (in-lines inport)])
    (+ acc (string->number n))))

(time-run bench benchargs)