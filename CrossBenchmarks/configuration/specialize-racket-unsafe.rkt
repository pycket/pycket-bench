#lang racket/base

(require racket/unsafe/ops racket/flonum racket/provide
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
    [flvector  FLOATvector]
    [flvector? FLOATvector?]
    [FXfx+     +]
    [FXfx-     -]
    [FXfx*     *])
  (filtered-out
   (lambda (name)
     (or
      (and (regexp-match? #rx"^unsafe-fx" name)
           (regexp-replace #rx"^unsafe-fx" name ""))
      (and (regexp-match? #rx"^unsafe-fl" name)
           (regexp-replace #rx"^unsafe-fl" name "FLOAT"))))
   (except-out (all-from-out racket/unsafe/ops)
               unsafe-fxvector-set!
               unsafe-fxvector-ref unsafe-fxvector-length
               unsafe-fx->fl unsafe-fl->fx unsafe-fxnot unsafe-fxand
               unsafe-fl+ unsafe-fl- unsafe-fl*
               unsafe-fx+ unsafe-fx- unsafe-fx*))
  (all-defined-out)
  zero? odd? even? /)

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    [(FLOAT+)     0.0]
    [(FLOAT+ x)   x]
    [(FLOAT+ x y) (unsafe-fl+ x y)]
    [(FLOAT+ x y ...) (unsafe-fl+ x (FLOAT+ y ...))]))

(define-syntax FLOAT-
  (syntax-rules ()
    [(FLOAT- x)   (unsafe-fl- 0.0 x)]
    [(FLOAT- x y) (unsafe-fl- x y)]
    [(FLOAT- x y ...) (unsafe-fl- x (FLOAT- y ...))]))

(define-syntax FLOAT*
  (syntax-rules ()
    [(FLOAT*)     1.0]
    [(FLOAT* x)   x]
    [(FLOAT* x y) (unsafe-fl* x y)]
    [(FLOAT* x y ...) (unsafe-fl* x (FLOAT* y ...))]))


(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (unsafe-fl< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (unsafe-fl> x 0.0))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (unsafe-fl= 0.0 x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))


(define-syntax FXfx+
  (syntax-rules ()
    [(FXfx+)     1]
    [(FXfx+ x)   x]
    [(FXfx+ x y) (unsafe-fx+ x y)]
    [(FXfx+ x y ...) (unsafe-fx+ x (FXfx+ y ...))]))

(define-syntax FXfx-
  (syntax-rules ()
    [(FXfx- x)   (unsafe-fx- 0 x)]
    [(FXfx- x y) (unsafe-fx- x y)]
    [(FXfx- x y ...) (unsafe-fx- x (FXfx- y ...))]))

(define-syntax FXfx*
  (syntax-rules ()
    [(FXfx*)     0]
    [(FXfx* x)   x]
    [(FXfx* x y) (unsafe-fx* x y)]
    [(FXfx* x y ...) (unsafe-fx* x (FXfx* y ...))]))


(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (unsafe-fx< 0 x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (unsafe-fx> 0 x))))

(define-syntax bitwise-or
  (syntax-rules ()
    ((bitwise-or x y) (unsafe-fxior x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (unsafe-fxand x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (unsafe-fxnot x))))
