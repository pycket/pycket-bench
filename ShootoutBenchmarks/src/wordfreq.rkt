; $Id: wordfreq-mzscheme.code,v 1.10 2006/06/21 15:05:34 bfulgham Exp $
;  http://shootout.alioth.debian.org/
;  wordfreq.mzscheme by Grzegorz Chrupaa
;  Updated and corrected by Brent Fulgham
;  Re-written by Matthew Flatt with some inspriation from the Python example

#lang racket/base
(require "time-run.rktl" racket/port)

(require mzlib/list)

(define (benchargs args)
  (let* ([file-name (if (= (vector-length args) 0)
                        (error "No file to process")
                        (vector-ref args 0))]
         [file      (open-input-file file-name)]
         [out       (open-output-string)])
    (copy-port file out)
    (open-input-string (get-output-string out))))


(define t (make-hash))

(define (register-word! s)
  (let ([s (string-downcase (bytes->string/utf-8 s))])
    (hash-set! t s (add1 (hash-ref t s (lambda () 0))))))

(define (bench in)
  (let loop ()
    (let ([m (regexp-match #rx#"[a-zA-Z]+" in)])
      (when m
        (register-word! (car m))
        (loop))))

  (for-each display
            (sort (hash-map
                   t
                   (lambda (word count)
                     (let ((count (number->string count)))
                       (format"~a~a ~a\n"
                              (make-string (- 7 (string-length count)) #\space)
                              count
                              word))))
                  string>?)))

(time-run bench benchargs)
