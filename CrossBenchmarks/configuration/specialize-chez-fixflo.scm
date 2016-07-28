;(compiler-switches 'fast-safe)
;
; Specialize fixnum and flonum arithmetic.

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
    ((FLOAT+ x ...) (fl+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (fl- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (fl* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (GENERIC/ x ...))))                ; FIXME

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (GENERIC= x y)))) ;fixme

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (GENERIC< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (GENERIC<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (GENERIC> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (GENERIC>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (GENERIC< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (GENERIC< 0.0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (GENERIC= 0.0 x)))) ;fixme

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (abs x))))                    ; FIXME

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))                    ; FIXME

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))                    ; FIXME

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))                  ; FIXME

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))                  ; FIXME

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))                ; FIXME

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))                ; FIXME

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))                ; FIXME

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

;(define-syntax quotient
;  (syntax-rules ()
;    ((quotient x ...) (quotient x ...))))       ; FIXME

;(define-syntax modulo
;  (syntax-rules ()
;    ((modulo x ...) (modulo x ...))))           ; FIXME

;(define-syntax remainder
;  (syntax-rules ()
;    ((remainder x ...) (remainder x ...))))     ; FIXME

;(define-syntax =
;  (syntax-rules ()
;    ((= x y) (fx= x y))))

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
    ((negative? x) (fxnegative? x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (fxpositive? x))))

(define-syntax zero?
  (syntax-rules ()
    ((zero? x) (fxzero? x))))

;(define-syntax odd?
;  (syntax-rules ()
;    ((odd? x) (odd? x))))                       ; FIXME

;(define-syntax even?
;  (syntax-rules ()
;    ((even? x) (even? x))))                     ; FIXME

(define-syntax bitwise-or
  (syntax-rules ()
    ((bitwise-or x y) (fxlogior x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (fxlogand x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (fxlognot x))))

