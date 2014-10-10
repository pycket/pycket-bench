#lang racket/base
(require "time-run.rktl" racket/port)

;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/

(define (all-counts len dna)
  (let ([table (make-hasheq)]
        [seq (make-string len)])
    (for ([s (in-range (- (string-length dna) len) -1 -1)])
      (string-copy! seq 0 dna s (+ s len))
      (let ([key (string->symbol seq)])
        (let ([b (or (hash-ref table key #f)
                     (let ([b (box 0)])
                       (hash-set! table key b)
                       b))])
          (set-box! b (add1 (unbox b))))))
    table))

(define (write-freqs table)
  (let* ([content (hash-map table (lambda (k v) (cons k (unbox v))))]
         [total (exact->inexact (apply + (map cdr content)))])
    (for ([a (sort content > #:key cdr)])
      (printf "~a ~a\n" 
              (car a) 
              (real->decimal-string (* 100 (/ (cdr a) total)) 3)))))

(define (write-one-freq table key)
  (let ([cnt (hash-ref table key (box 0))])
    (printf "~a\t~a\n" (unbox cnt) key)))


(define (benchargs args)
  (let* ([file-name (if (= (vector-length args) 0)
                        (error "No file to process")
                        (vector-ref args 0))]
         [file      (open-input-file file-name)]
         [out       (open-output-string)])
    (copy-port file out)
    (open-input-string (get-output-string out))))

(define (bench in)

  (define dna
    (let ()
      ;; Skip to ">THREE ..."
      (regexp-match #rx#"(?m:^>THREE.*$)" in)
      (let ([s (open-output-string)])
        ;; Copy everything but newlines to s:
        (for ([l (in-bytes-lines in)])
          (write-bytes l s))
        ;; Extract the string from s:
        (string-upcase (get-output-string s)))))


  ;; 1-nucleotide counts:
  (write-freqs (all-counts 1 dna))
  (newline)

  ;; 2-nucleotide counts:
  (write-freqs (all-counts 2 dna))
  (newline)

  ;; Specific sequences:
  (for ([seq '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT")]) 
    (write-one-freq (all-counts (string-length seq) dna)
                    (string->symbol seq))))

(time-run bench benchargs)