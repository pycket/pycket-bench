#lang racket/base

(provide (all-defined-out)
         positive? negative? zero?
         quotient modulo remainder odd? even? min max
         > < >= <= = / - + *)

; Don't specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-vector n 0.0))
    ((FLOATmake-vector n init) (make-vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (negative? x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (positive? x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (zero? x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (abs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

; Generic arithmetic.

(define-syntax GENERIC+
  (syntax-rules ()
    ((GENERIC+ x ...) (+ x ...))))

(define-syntax GENERIC-
  (syntax-rules ()
    ((GENERIC- x ...) (- x ...))))

(define-syntax GENERIC*
  (syntax-rules ()
    ((GENERIC* x ...) (* x ...))))

(define-syntax GENERIC/
  (syntax-rules ()
    ((GENERIC/ x ...) (/ x ...))))

(define-syntax GENERICquotient
  (syntax-rules ()
    ((GENERICquotient x y) (quotient x y))))

(define-syntax GENERICremainder
  (syntax-rules ()
    ((GENERICremainder x y) (remainder x y))))

(define-syntax GENERICmodulo
  (syntax-rules ()
    ((GENERICmodulo x y) (modulo x y))))

(define-syntax GENERIC=
  (syntax-rules ()
    ((GENERIC= x y) (= x y))))

(define-syntax GENERIC<
  (syntax-rules ()
    ((GENERIC< x y) (< x y))))

(define-syntax GENERIC<=
  (syntax-rules ()
    ((GENERIC<= x y) (<= x y))))

(define-syntax GENERIC>
  (syntax-rules ()
    ((GENERIC> x y) (> x y))))

(define-syntax GENERIC>=
  (syntax-rules ()
    ((GENERIC>= x y) (>= x y))))

(define-syntax GENERICexpt
  (syntax-rules ()
    ((GENERICexpt x y) (expt x y))))
