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
(define-syntax list (make-rename-transformer #'mlist))
(define-syntax set-car! (make-rename-transformer #'set-mcar!))
(define-syntax set-cdr! (make-rename-transformer #'set-mcdr!))
(define-syntax car (make-rename-transformer #'mcar))
(define-syntax cdr (make-rename-transformer #'mcdr))
(define-syntax cons (make-rename-transformer #'mcons))
(define-syntax length (make-rename-transformer #'mlength))
(define-syntax pair? (make-rename-transformer #'mpair?))
(define-syntax map (make-rename-transformer #'mmap))
;; (define-syntax apply (make-rename-transformer #'mapply))
(define-syntax for-each (make-rename-transformer #'mfor-each))
;; (define-syntax string->list (make-rename-transformer #'string->mlist))
;; (define-syntax list->string (make-rename-transformer #'mlist->string))
;; (define-syntax vector->list (make-rename-transformer #'vector->mlist))
;; (define-syntax list->vector (make-rename-transformer #'mlist->vector))
(define-syntax list? (make-rename-transformer #'mlist?))
(define-syntax append (make-rename-transformer #'mappend))
(define-syntax reverse (make-rename-transformer #'mreverse))
(define-syntax list-tail (make-rename-transformer #'mlist-tail))
(define-syntax list-ref (make-rename-transformer #'mlist-ref))
(define-syntax memq (make-rename-transformer #'mmemq))
(define-syntax memv (make-rename-transformer #'mmemv))
(define-syntax member (make-rename-transformer #'mmember))
(define-syntax assq (make-rename-transformer #'massq))
(define-syntax assv (make-rename-transformer #'massv))
(define-syntax assoc (make-rename-transformer #'massoc))
(require (prefix-in r5: r5rs))
(define-syntax quote (make-rename-transformer #'r5:quote))
(define-syntax-rule (cadr e) (car (cdr e)))
(define-syntax-rule (caddr e) (car (cdr (cdr e))))
(define-syntax-rule (cadddr e) (car (cdr (cdr (cdr e)))))
;; (define-syntax read (make-rename-transformer #'mread))
;; (define-syntax write (make-rename-transformer #'mwrite))
;; (define-syntax display (make-rename-transformer #'mdisplay))
;; (define-syntax eval (make-rename-transformer #'meval))

;; (define (string->mlist s) (list->mlist (string->list s)))
;; (define (mlist->string s) (list->string (mlist->list s)))

;; (define (vector->mlist s) (list->mlist (vector->list s)))
;; (define (mlist->vector s) (list->vector (mlist->list s)))

;; (define mapply
;;   (case-lambda
;;     [(f l) (apply f (mlist->list l))]
;;     [(f l0 . l)
;;      (apply f (let loop ([l (cons l0 l)])
;;                 (if (null? (cdr l))
;;                     (mlist->list (car l))
;;                     (cons (car l) (loop (cdr l))))))]))

;; ;; --------------------------------------------------

;; (define mread
;;   (case-lambda
;;     [() (mread (current-input-port))]
;;     [(port) (let loop ([v (read port)])
;;               (cond
;;                [(pair? v) (mcons (loop (car v)) (loop (cdr v)))]
;;                [(vector? v) (list->vector
;;                              (map loop (vector->list v)))]
;;                [else v]))]))

;; (define mwrite
;;   (case-lambda
;;     [(v) (mwrite v (current-output-port))]
;;     [(v port) (parameterize ([print-mpair-curly-braces #f])
;;                 (write v port))]))

;; (define mdisplay
;;   (case-lambda
;;     [(v) (mdisplay v (current-output-port))]
;;     [(v port) (parameterize ([print-mpair-curly-braces #f])
;;                 (display v port))]))

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
           (printf "RESULT-cpu: ~a.0\nRESULT-total: ~a.0\nRESULT-gc: ~a.0\n"
                   cpu user gc)
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
   (call-with-output-file filename proc #:exists 'truncate))

;------------------------------------------------------------------------------
