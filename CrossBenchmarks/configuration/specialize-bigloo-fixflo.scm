;(define-syntax FLOATvector-const
;  (syntax-rules ()
;    ((FLOATvector-const x ...) '#f64(x ...))))

(def-macro (FLOATvector-const . lst)   `',(list->f64vector lst))
(def-macro (FLOATvector? x)            `(f64vector? ,x))
(def-macro (FLOATvector . lst)         `(f64vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-f64vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(f64vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(f64vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(f64vector-length ,v))


(def-macro (nuc-const . lst)
  `',(list->vector
       (map (lambda (x)
              (if (vector? x)
                (list->f64vector (vector->list x))
                x))
            lst)))

(define-syntax FLOAT+
  (syntax-rules ()
    [(FLOAT+)     0.0]
    [(FLOAT+ x)   x]
    [(FLOAT+ x y) (+fl x y)]
    [(FLOAT+ x y ...) (+fl x (FLOAT+ y ...))]))

(define-syntax FLOAT-
  (syntax-rules ()
    [(FLOAT- x)   (negfl x)]
    [(FLOAT- x y) (-fl x y)]
    [(FLOAT- x y ...) (-fl x (FLOAT- y ...))]))

(define-syntax FLOAT*
  (syntax-rules ()
    [(FLOAT*)     1.0]
    [(FLOAT* x)   x]
    [(FLOAT* x y) (*fl x y)]
    [(FLOAT* x y ...) (*fl x (FLOAT* y ...))]))

(def-macro (FLOAT/ . lst)
  (cond ((null? (cdr lst)) `(/fl 1.0 ,(car lst)))
        (else              `(/fl ,(car lst) (FLOAT/ ,@(cdr lst))))))

(def-macro (FLOAT= . lst)  `(=fl ,@lst))
(def-macro (FLOAT< . lst)  `(<fl ,@lst))
(def-macro (FLOAT<= . lst) `(<=fl ,@lst))
(def-macro (FLOAT> . lst)  `(>fl ,@lst))
(def-macro (FLOAT>= . lst) `(>=fl ,@lst))
(def-macro (FLOATnegative? . lst) `(negativefl? ,@lst))
(def-macro (FLOATpositive? . lst) `(positivefl? ,@lst))
(def-macro (FLOATzero? . lst)     `(zerofl? ,@lst))
(def-macro (FLOATabs . lst) `(abs ,@lst))
(def-macro (FLOATsin . lst) `(sin ,@lst))
(def-macro (FLOATcos . lst) `(cos ,@lst))
(def-macro (FLOATatan . lst) `(atan ,@lst))
(def-macro (FLOATsqrt . lst) `(sqrt ,@lst))
(def-macro (FLOATmin . lst) `(minfl ,@lst))
(def-macro (FLOATmax . lst) `(maxfl ,@lst))
(def-macro (FLOATround . lst) `(roundfl ,@lst))
(def-macro (FLOATinexact->exact . lst) `(inexact->exact ,@lst))

; Generic arithmetic.
(define GENERIC+ (lambda (x y) x))
(define GENERIC- (lambda (x y) x))
(define GENERIC* (lambda (x y) x))
(define GENERIC/ (lambda (x y) x))
(define GENERICquotient (lambda (x y) x))
(define GENERICremainder (lambda (x y) x))
(define GENERICmodulo (lambda (x y) x))
(define GENERIC= (lambda (x y) x))
(define GENERIC< (lambda (x y) x))
(define GENERIC<= (lambda (x y) x))
(define GENERIC> (lambda (x y) x))
(define GENERIC>= (lambda (x y) x))
(define GENERICexpt (lambda (x y) x))

(set! GENERIC+ +)
(set! GENERIC- -)
(set! GENERIC* *)
(set! GENERIC/ /)
(set! GENERICquotient quotient)
(set! GENERICremainder remainder)
(set! GENERICmodulo modulo)
(set! GENERIC= =)
(set! GENERIC< <)
(set! GENERIC<= <=)
(set! GENERIC> >)
(set! GENERIC>= >=)
(set! GENERICexpt expt)


(define-syntax +
  (syntax-rules ()
    [(+)     0]
    [(+ x)   x]
    [(+ x y) (+fx x y)]
    [(+ x y ...) (+fx x (+ y ...))]))

(define-syntax -
  (syntax-rules ()
    [(- x)   (negfx x)]
    [(- x y) (-fx x y)]
    [(- x y ...) (-fx x (- y ...))]))

(define-syntax *
  (syntax-rules ()
    [(*)     1]
    [(* x)   x]
    [(* x y) (*fx x y)]
    [(* x y ...) (*fx x (* y ...))]))

(define-syntax /
  (syntax-rules ()
    [(/ x)   (/fx 1 x)]
    [(/ x y) (/fx x y)]
    [(/ x y ...) (/fx x (/ y ...))]))


(def-macro (quotient . lst) `(quotientfx ,@lst))
(def-macro (modulo . lst) `(modulofx ,@lst))
(def-macro (remainder . lst) `(remainderfx ,@lst))
(def-macro (= . lst)  `(=fx ,@lst))
(def-macro (< . lst)  `(<fx ,@lst))
(def-macro (<= . lst) `(<=fx ,@lst))
(def-macro (> . lst)  `(>fx ,@lst))
(def-macro (>= . lst) `(>=fx ,@lst))
(def-macro (negative? . lst) `(negativefx? ,@lst))
(def-macro (positive? . lst) `(positivefx? ,@lst))
(def-macro (zero? . lst) `(zerofx? ,@lst))
(def-macro (odd? . lst) `(oddfx? ,@lst))
(def-macro (even? . lst) `(evenfx? ,@lst))
(def-macro (bitwise-or . lst) `(bit-or ,@lst))
(def-macro (bitwise-and . lst) `(bit-and ,@lst))
(def-macro (bitwise-not . lst) `(bit-not ,@lst))
