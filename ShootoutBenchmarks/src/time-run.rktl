#lang racket/base
(provide time-run)

(define (benchargs-default args)
  (if (= (vector-length args) 0) 1 (string->number (vector-ref args 0))))

(define (time-run bench . argsl)
  (let* ([benchargs (if (null? argsl)
                        benchargs-default
                        (car argsl))]
         [args      (benchargs (current-command-line-arguments))])
    (let-values ([(v cpu user gc) (time-apply bench (list args))])
      (fprintf (current-error-port)
              "\n\nRESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
              cpu gc user)
      (apply values v))))

;; Local Variables:
;; mode: scheme
;; geiser-scheme-implementation: racket
;; End:
