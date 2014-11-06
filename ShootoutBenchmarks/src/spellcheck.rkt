;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;;
;;; spellcheck benchmark

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

(define (bench in)

  (define dict (make-hash))

  (with-input-from-file "Usr.Dict.Words"
    (lambda ()
      (let loop ()
        (let ([r (read-bytes-line)])
          (unless (eof-object? r)
            (hash-set! dict r #t)
            (loop))))))

  (let loop ()
    (let ([w (read-bytes-line in)])
      (unless (eof-object? w)
        (unless (hash-ref dict w (lambda () #f))
          (printf "~a\n" w))
        (loop)))))

(time-run bench benchargs)
