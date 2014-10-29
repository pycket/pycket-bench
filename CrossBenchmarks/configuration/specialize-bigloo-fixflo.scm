
;(define-syntax FLOATvector-const
;  (syntax-rules ()
;    ((FLOATvector-const x ...) '#f64(x ...))))

(define-macro (FLOATvector-const . lst)
  `',(list->f64vector lst))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (f64vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (f64vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-f64vector n 0.0))
    ((FLOATmake-vector n init) (make-f64vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (f64vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (f64vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (f64vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (+fl x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (-fl x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (*fl x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (/fl x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (=fl x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (<fl x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (<=fl x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (>fl x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (>=fl x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (negativefl? x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (positivefl? x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (zerofl? x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (absfl x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sinfl x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cosfl x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atanfl x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrtfl x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (minfl x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (maxfl x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (roundfl x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

; Generic arithmetic.
(define (GENERIC+ x y) (+ x y))
(define (GENERIC- x y) (- x y))
(define (GENERIC* x y) (* x y))
(define (GENERIC/ x y) (/ x y))
(define (GENERICquotient x y) (quotient x y))
(define (GENERICremainder x y) (remainder x y))
(define (GENERICmodulo x y) (modulo x y))
(define (GENERIC= x y) (= x y))
(define (GENERIC< x y) (< x y))
(define (GENERIC<= x y) (<= x y))
(define (GENERIC> x y) (> x y))
(define (GENERIC>= x y) (>= x y))
(define (GENERICexpt x y) (expt x y))

(define-syntax +
  (syntax-rules ()
    ((+ x ...) (+fx x ...))))

(define-syntax -
  (syntax-rules ()
    ((- x ...) (-fx x ...))))

(define-syntax *
  (syntax-rules ()
    ((* x ...) (*fx x ...))))

(define-syntax quotient
  (syntax-rules ()
    ((quotient x ...) (quotientfx x ...))))

(define-syntax modulo
  (syntax-rules ()
    ((modulo x ...) (modulofx x ...))))

(define-syntax remainder
  (syntax-rules ()
    ((remainder x ...) (remainderfx x ...))))

(define-syntax =
  (syntax-rules ()
    ((= x y) (=fx x y))))

(define-syntax <
  (syntax-rules ()
    ((< x y) (<fx x y))))

(define-syntax <=
  (syntax-rules ()
    ((<= x y) (<=fx x y))))

(define-syntax >
  (syntax-rules ()
    ((> x y) (>fx x y))))

(define-syntax >=
  (syntax-rules ()
    ((>= x y) (>=fx x y))))

(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (negativefx? x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (positivefx? x))))

(define-syntax zero?
  (syntax-rules ()
    ((zero? x) (zerofx? x))))

(define-syntax odd?
  (syntax-rules ()
    ((odd? x) (oddfx? x))))

(define-syntax even?
  (syntax-rules ()
    ((even? x) (evenfx? x))))

(define-syntax bitwise-or
  (syntax-rules ()
    ((bitwise-or x y) (bit-or x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (bit-and x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (bit-not x))))

