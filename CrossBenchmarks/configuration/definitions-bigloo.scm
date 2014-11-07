(module bench)

;------------------------------------------------------------------------------
; customized timer
; in ReBench TestVMPerformance format
(define-macro (timer . es)
  `(let ()
      (multiple-value-bind (res rtime stime utime)
          (time (lambda () ,@es))
          (print "RESULT-cpu: " (GENERIC* 1.0 (GENERIC+ stime utime)) "\n"
                 "RESULT-gc: 0.0\n"
                 "RESULT-total: " (GENERIC* 1.0 rtime) "\n")
        res)))

;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (timer (run-bench name count ok? run))))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline))))
  (exit 0))

(define (fatal-error . args)
  (apply error #f args))

(define (call-with-output-file/truncate filename proc)
  (truncate-file filename 0)
  (call-with-output-file filename proc))

(define (open-output-file/truncate filename)
  (truncate-file filename 0)
  (open-output-file filename))

(define (bigloo-main argv)
    (apply main argv))

(define-macro (def-macro form . body)
  `(define-macro ,form (let () ,@body)))

;

