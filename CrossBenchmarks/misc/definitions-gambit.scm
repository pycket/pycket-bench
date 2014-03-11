;------------------------------------------------------------------------------
; customized timer
; in ReBench TestVMPerformance format
(define-macro (time . es)
  `(let ((t (cpu-time))
        (r (real-time)))
     (call-with-values
         (lambda () ,@es)
       (lambda vals
         (let* ((t* (cpu-time))
                (r* (real-time))
                (d (number->string (* (- t* t) 1000)))
                (rd (number->string (* (- r* r) 1000))))
           (display (string-append "RESULT-cpu: " d
                                   "\nRESULT-total: " rd
                                   "\nRESULT-gc: 0.0\n"))
           (apply values vals))))))
;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline))))
  (exit 0))

(define (fatal-error . args)
  (for-each display args)
  (newline)
  (exit 1))

 (define (call-with-output-file/truncate filename proc)
   (call-with-output-file filename proc))

;-----------------------------------------------------------------------------
(declare (standard-bindings) (block))
