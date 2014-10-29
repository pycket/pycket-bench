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
    [flvector? FLOATvector?])
  (filtered-out
   (lambda (name)
     (or
      (and (regexp-match? #rx"^unsafe-fx]" name)
           (regexp-replace #rx"^unsafe-fx" name ""))
      (and (regexp-match? #rx"^unsafe-fl" name)
           (regexp-replace #rx"^unsafe-fl" name "FLOAT"))))
   (except-out (all-from-out racket/unsafe/ops)
               unsafe-fx->fl unsafe-fl->fx unsafe-fxnot))
  (all-defined-out))

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))


(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (unsafe-fl< 0 x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (unsafe-fl> 0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (unsafe-fl= 0 x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

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
