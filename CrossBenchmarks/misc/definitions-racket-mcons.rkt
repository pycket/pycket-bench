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

(define (-list->mlist l)
  (cond [(null? l) l]
        [else (mcons (car l) (-list->mlist (cdr l)))]))
(define list->mlist -list->mlist)

  (define-syntax (r5:lambda stx)
    ;; Convert rest-arg list to mlist, and use r5rs:body:
    (syntax-case stx ()
      [(_ (id ...) . body)
       (syntax/loc stx (#%plain-lambda (id ...) . body))]
      [(_ (id ... . rest) . body)
       (syntax/loc stx
         (#%plain-lambda (id ... . rest)
                         (let ([rest (-list->mlist rest)])
                           (begin . body))))]))

  (define-syntax (r5:define stx)
    ;; Use r5rs:lambda
    (syntax-case stx ()
      [(_ (id . args) . body)
       (with-syntax ([proc
                      (syntax/loc stx
                        (r5:lambda args . body))])
         (syntax/loc stx
           (define id proc)))]
      [(_ . rest)
       (syntax/loc stx
         (define . rest))]))


;------------------------------------------------------------------------------
;(require (prefix-in r5: r5rs))
(define-syntax list (make-rename-transformer #'mlist))
(define-syntax set-car! (make-rename-transformer #'set-mcar!))
(define-syntax set-cdr! (make-rename-transformer #'set-mcdr!))
(define-syntax car (make-rename-transformer #'mcar))
(define-syntax cdr (make-rename-transformer #'mcdr))
(define-syntax cons (make-rename-transformer #'mcons))
(define-syntax length (make-rename-transformer #'mlength))
(define-syntax pair? (make-rename-transformer #'mpair?))
(define-syntax map (make-rename-transformer #'mmap))
(define-syntax apply (make-rename-transformer #'r5:apply))
(define-syntax for-each (make-rename-transformer #'mfor-each))
(define-syntax string->list (make-rename-transformer #'r5:string->list))
(define-syntax list->string (make-rename-transformer #'r5:list->string))
(define-syntax vector->list (make-rename-transformer #'r5:vector->list))
(define-syntax list->vector (make-rename-transformer #'r5:list->vector))
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
(define-syntax quote (make-rename-transformer #'r5:quote))
(define-syntax quasiquote (make-rename-transformer #'r5:quasiquote))
(define-syntax unquote (make-rename-transformer #'r5:unquote))
(define-syntax (cadr stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (cdr e)))]))
(define-syntax (caar stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (car e)))]))
(define-syntax (cddr stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (cdr (cdr e)))]))
(define-syntax (cdar stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (cdr (car e)))]))
(define-syntax (cadar stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (cdr (car e))))]))
(define-syntax (caddr stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (cdr (cdr e))))]))
(define-syntax (cdddr stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (cdr (cdr (cdr e))))]))
(define-syntax (caddar stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (cdr (cdr (car e)))))]))
(define-syntax (cadddr stx)
  (syntax-case stx ()
    [(i e) #'(#%app i e)]
    [_ #'(lambda (e) (car (cdr (cdr (cdr e)))))]))
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
  (#%plain-lambda (stx)
    (syntax-case stx ()
      [(_ expr1 expr ...)
       (syntax/loc
           stx
         (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
           (printf "RESULT-cpu: ~a.0\nRESULT-total: ~a.0\nRESULT-gc: ~a.0\n"
                   cpu user gc)
           (r5:apply values (-list->mlist v))))])))
;------------------------------------------------------------------------------
(define (run-bench name count ok? run)
  (let loop ((i 0) (result (r5:list 'undefined)))
    (if (< i count)
      (begin (printf "loop: ~a\n" i) (loop (+ i 1) (run)))
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
   (mz:call-with-output-file filename proc 'binary 'truncate))

;------------------------------------------------------------------------------
