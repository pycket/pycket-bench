;------------------------------------------------------------------------------
(define (run-bench name count ok? run)
  (let loop ((i 0) (result (r5:list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(r5:define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (r5:apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (when (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline)))))

(define (fatal-error . args)
  (r5:apply error #f args))

 (define (call-with-output-file/truncate filename proc)
   (k:call-with-output-file filename proc 'binary 'truncate))

;------------------------------------------------------------------------------
