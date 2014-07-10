;------------------------------------------------------------------------------
;one-armed if.
(define-syntax if
  (syntax-rules ()
    ((if expr true-branch)
     (when expr true-branch))
    ((if expr true-branch false-branch)
     (cond
       (expr true-branch)
       (else false-branch)))))
;------------------------------------------------------------------------------
; customized timer
; in ReBench TestVMPerformance format
(define-syntax time
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr1 expr ...)
       (syntax/loc
           stx
         (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
           (printf "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
                   cpu gc user)
           (apply values v)))])))
;------------------------------------------------------------------------------
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
   (mz:call-with-output-file filename proc 'binary 'truncate))

;------------------------------------------------------------------------------
