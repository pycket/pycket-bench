(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define warmup-iters
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 0
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

(define (warmup-loop name count ok? run)
  (define (loop n)
    (if (= n 0)
      (void)
      (begin
        (run-bench name count ok? run)
        (loop (- n 1)))))
  (loop warmup-iters))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (begin
                   (warmup-loop name count ok? run)
                   (time (run-bench name count ok? run)))))
    (when (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline)))))

(define (fatal-error . args)
  (apply error #f args))

(define (call-with-output-file/truncate filename proc)
 (call-with-output-file filename proc #:mode 'binary #:exists 'truncate))

(define (open-output-file/truncate filename)
  (open-output-file filename #:mode 'binary #:exists 'truncate))

;------------------------------------------------------------------------------
