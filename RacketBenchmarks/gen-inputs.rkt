#lang racket
(require racket/runtime-path)
(define-runtime-path here ".")


(define (mk-regexmatch-input n)
  (let ([f (build-path (string-append "regexmatch-" (number->string n)))])
    (unless (file-exists? f)
      (printf "Building regexmatch ~a output for input: ~a\n" n f)
      (with-output-to-file f
        (lambda ()
          ;; taken from heapsort.rkt
          (define IM   139968)
          (define IA     3877)
          (define IC    29573)
          (define LAST 42)
          (define (gen_random max)
            (set! LAST (modulo (+ (* LAST IA) IC) IM))
            (/ (* max LAST) IM))
          (define (random-int max) (inexact->exact (round (gen_random max))))
          ;; this can generate malformed phone numbers (with a 1 or 2 number
          ;; area code, for instance) but that's fine, the regex just won't
          ;; match
          (let loop ((n n))
            (unless (zero? n)
              (printf (format "(~a) ~a-~a\n"
                              (random-int 1000)
                              (random-int 1000)
                              (random-int 10000)))
              (loop (sub1 n)))))))
    f))

(define (mk-fasta-input n)
  (let ([f (build-path (string-append "fasta-" (number->string n)))])
    (unless (file-exists? f)
      (printf "Building FASTA ~a output for input: ~a\n" n f)
      (with-output-to-file f
        (lambda ()
          (parameterize ([current-load-relative-directory here])
            ((dynamic-require "src/fasta.rkt" 'generate) n)))))
    f))

(define-runtime-path sumcol-input "sumcol-input.txt")

(define (mk-sumcol-input n [moments? #f])
  (let ([f (build-path (string-append (if moments? "moments-" "sumcol-")
                                      (number->string n)))])
    (unless (file-exists? f)
      (printf "Building ~a ~a input: ~a\n"
              (if moments? "moments" "sumcol") n f)
      (let ([c (with-input-from-file sumcol-input
                 (lambda ()
                   (if moments?
                       (apply string-append ; like sumcol, but with floats
                              (map (lambda (x)
                                     (string-append
                                      (number->string (exact->inexact x))
                                      "\n"))
                                   (port->list)))
                       (read-bytes 10000))))])
        (with-output-to-file f
          (lambda ()
            (let loop ([n n])
              (unless (zero? n)
                (printf "~a" c)
                (loop (sub1 n))))))))
    f))

(define (mk-moments-input n)
  (mk-sumcol-input n #t))

(mk-moments-input 2000)
(mk-regexmatch-input 1000000)
(mk-fasta-input 1000000)
(mk-fasta-input 30000000)
(mk-fasta-input 500000)
(mk-sumcol-input 3000)
(mk-sumcol-input 10000)
(mk-sumcol-input 20000)
(mk-sumcol-input 10000)
