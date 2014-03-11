(begin

;(require racket/unsafe/ops)

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
    ((FLOATvector-ref v i) (unsafe-flvector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (unsafe-flvector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (unsafe-flvector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (unsafe-fl+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (unsafe-fl- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (unsafe-fl* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (unsafe-fl/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (unsafe-fl= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (unsafe-fl< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (unsafe-fl<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (unsafe-fl> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (unsafe-fl>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (unsafe-fl< 0 x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (unsafe-fl> 0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (unsafe-fl= 0 x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (unsafe-flabs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (unsafe-flsin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (unsafe-flcos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (unsafe-flatan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (unsafe-flsqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (unsafe-flmin x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (unsafe-flmax x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (unsafe-flround x))))

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
    ((+ x ...) (unsafe-fx+ x ...))))

(define-syntax -
  (syntax-rules ()
    ((- x ...) (unsafe-fx- x ...))))

(define-syntax *
  (syntax-rules ()
    ((* x ...) (unsafe-fx* x ...))))

(define-syntax quotient
  (syntax-rules ()
    ((quotient x ...) (unsafe-fxquotient x ...))))

(define-syntax modulo
  (syntax-rules ()
    ((modulo x ...) (unsafe-fxmodulo x ...))))

(define-syntax remainder
  (syntax-rules ()
    ((remainder x ...) (unsafe-fxremainder x ...))))

(define-syntax =
  (syntax-rules ()
    ((= x y) (unsafe-fx= x y))))

(define-syntax <
  (syntax-rules ()
    ((< x y) (unsafe-fx< x y))))

(define-syntax <=
  (syntax-rules ()
    ((<= x y) (unsafe-fx<= x y))))

(define-syntax >
  (syntax-rules ()
    ((> x y) (unsafe-fx> x y))))

(define-syntax >=
  (syntax-rules ()
    ((>= x y) (unsafe-fx>= x y))))

(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (unsafe-fx< 0 x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (unsafe-fx> 0 x))))

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
    ((bitwise-or x y) (unsafe-fxior x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (unsafe-fxand x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (unsafe-fxnot x))))
)
