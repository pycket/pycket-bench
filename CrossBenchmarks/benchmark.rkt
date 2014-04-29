(require racket/vector)
(define args (current-command-line-arguments))
(if (and (> (vector-length args) 0) (string=? (vector-ref args 0) "--mcons"))
    (begin
      (set! args (vector-drop args 1))
      (load "misc/definitions-racket-mcons.rkt"))
    (load "misc/definitions-racket.rkt"))

(define (make-benchmark)
  (let ([arguments  args]
        [iterations #f])
    (define (arg-nr n)
      (vector-ref arguments n))
    (define (specialized?)
      (eqv? #\- (string-ref (arg-nr 0) 0)))
    (define (benchmark-name)
      (if (specialized?) (arg-nr 1) (arg-nr 0)))
    (define (benchmark-specialization)
      (if (specialized?)
          (string-downcase (substring (arg-nr 0) 1))
          "nothing"))
    (define (benchmark-iterations!)
      (let* ([benchmark (benchmark-name)]
             [arglist   (vector->list arguments)]
             [restlist  (if (specialized?) (cddr arglist) (cdr arglist))])
        (if (null? restlist)
            (begin (load "misc/iterations.rkt") (void))
            (begin (set! iterations (string->number (car restlist))) (void)))))
    (define (benchmark-additional-args)
      (let* ([arglist   (vector->list arguments)]
             [restlist  (if (specialized?) (cdddr arglist) (cddr arglist))])
        restlist))
    (define (go)
      (load (string-append "misc/specialize-racket-" (benchmark-specialization) ".rkt"))
      (benchmark-iterations!)
      ;(when (iterations) laters. ; (string->symbol (cadr (syntax->datum (expand f))))
      (load (string-append "src/" (benchmark-name) ".scm"))
      (main))
    (lambda (sym)
      (cond [(eq? sym 'name)            (benchmark-name)]
            [(eq? sym 'make-iterations) (benchmark-iterations!)]
            [(eq? sym 'specialization)  (benchmark-specialization)]
            [(eq? sym 'additional-args) (benchmark-additional-args)]
            [(eq? sym 'go)              (go)]
            [else (error "unknown symbol" sym)]))))
((make-benchmark) 'go)

