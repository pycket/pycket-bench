(begin
(require racket/fixnum)
(require racket/flonum)
; Specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (flvector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (flvector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-flvector n 0.0))
    ((FLOATmake-vector n init) (make-flvector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (flvector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (flvector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (flvector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (fl+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (fl- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (fl* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (fl/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (fl= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (fl< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (fl<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (fl> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (fl>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (fl< 0.0 x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (fl> 0.0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (fl= 0.0 x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (flabs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (flsin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (flcos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (flatan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (flsqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (flmin x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (flmax x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (flround x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

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
    ((+ x ...) (fx+ x ...))))

(define-syntax -
  (syntax-rules ()
    ((- x ...) (fx- x ...))))

(define-syntax *
  (syntax-rules ()
    ((* x ...) (fx* x ...))))

(define-syntax quotient
  (syntax-rules ()
    ((quotient x ...) (fxquotient x ...))))

(define-syntax modulo
  (syntax-rules ()
    ((modulo x ...) (fxmodulo x ...))))

(define-syntax remainder
  (syntax-rules ()
    ((remainder x ...) (fxremainder x ...))))

(define-syntax =
  (syntax-rules ()
    ((= x y) (fx= x y))))

(define-syntax <
  (syntax-rules ()
    ((< x y) (fx< x y))))

(define-syntax <=
  (syntax-rules ()
    ((<= x y) (fx<= x y))))

(define-syntax >
  (syntax-rules ()
    ((> x y) (fx> x y))))

(define-syntax >=
  (syntax-rules ()
    ((>= x y) (fx>= x y))))

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
)