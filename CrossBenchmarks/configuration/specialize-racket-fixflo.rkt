#lang racket/base

(require racket/fixnum racket/flonum racket/provide
         (for-syntax racket/base))
(provide
  (rename-out
    [+         GENERIC+]
    [-         GENERIC-]
    [*         GENERIC*]
    [/         GENERIC/]
    [quotient  GENERICquotient]
    [remainder GENERICremainder]
    [modulo    GENERICmodulo]
    [=         GENERIC=]
    [<         GENERIC<]
    [<=        GENERIC<=]
    [>         GENERIC>]
    [>=        GENERIC>=]
    [expt      GENERICexpt]
    [make-flvector FLOATmake-vector]
    [FXfx+     +]
    [FXfx-     -]
    [FXfx*     *])
  (filtered-out
   (lambda (name) (regexp-replace #rx"^fl" name "FLOAT"))
   (except-out (all-from-out racket/flonum)
               fl+ fl- fl*))
  (filtered-out
   (lambda (name) (regexp-replace #rx"^fx" name ""))
   (except-out (all-from-out racket/fixnum)
               fx->fl fl->fx fxnot fxand fx+ fx* fx-
               fxvector fxvector? fxvector-set! fxvector-ref fxvector-length))
  (all-defined-out)
  zero? odd? even? /)
; Specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) (flvector x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    [(FLOAT+)     0.0]
    [(FLOAT+ x)   x]
    [(FLOAT+ x y) (fl+ x y)]
    [(FLOAT+ x y ...) (fl+ x (FLOAT+ y ...))]))

(define-syntax FLOAT-
  (syntax-rules ()
    [(FLOAT- x)   (fl- 0.0 x)]
    [(FLOAT- x y) (fl- x y)]
    [(FLOAT- x y ...) (fl- x (FLOAT- y ...))]))

(define-syntax FLOAT*
  (syntax-rules ()
    [(FLOAT*)     1.0]
    [(FLOAT* x)   x]
    [(FLOAT* x y) (fl* x y)]
    [(FLOAT* x y ...) (fl* x (FLOAT* y ...))]))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (fl< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (fl> x 0.0))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (fl= 0.0 x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (fl->exact-integer x))))


(define-syntax FXfx+
  (syntax-rules ()
    [(FXfx+)     0]
    [(FXfx+ x)   x]
    [(FXfx+ x y) (fx+ x y)]
    [(FXfx+ x y ...) (fx+ x (FXfx+ y ...))]))

(define-syntax FXfx-
  (syntax-rules ()
    [(FXfx- x)   (fx- 0 x)]
    [(FXfx- x y) (fx- x y)]
    [(FXfx- x y ...) (fx- x (FXfx- y ...))]))

(define-syntax FXfx*
  (syntax-rules ()
    [(FXfx*)     0]
    [(FXfx* x)   x]
    [(FXfx* x y) (fx* x y)]
    [(FXfx* x y ...) (fx* x (FXfx* y ...))]))


(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (fx< 0 x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (fx> 0 x))))

;(define-syntax zero?
;  (syntax-rules ()
;    ((zero? x) (fxzero? x))))

;(define-syntax odd?
;  (syntax-rules ()
;    ((odd? x) (fxodd? x))))

;(define-syntax even?
;  (syntax-rules ()
;    ((even? x) (fxeven? x))))

(define-syntax bitwise-or
  (syntax-rules ()
    ((bitwise-or x y) (fxior x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (fxand x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (fxnot x))))
