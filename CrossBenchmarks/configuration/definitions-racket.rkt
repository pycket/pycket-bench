(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
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
