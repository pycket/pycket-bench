#lang racket/base
;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf

(require "time-run.rktl")

(require racket/require (for-syntax racket/base))

(struct *leaf (val))
(struct *node *leaf (left right))

(define-syntax leaf  (make-rename-transformer #'*leaf))
(define-syntax leaf? (make-rename-transformer #'*leaf?))
(define-syntax node  (make-rename-transformer #'*node))
(define-syntax node? (make-rename-transformer #'*node?))
(define-syntax-rule (leaf-val l)   (*leaf-val l))
(define-syntax-rule (node-left n)  (*node-left n))
(define-syntax-rule (node-right n) (*node-right n))

(define (make item d)
  (if (= d 0)
    (leaf item)
    (let ([item2 (* item 2)] [d2 (- d 1)])
      (node item (make (- item2 1) d2) (make item2 d2)))))

(define (check t)
  (let loop ([t t] [acc 0])
    (let ([acc (+ (leaf-val t) acc)])
      (if (node? t)
        (loop (node-left t)
              (- acc (loop (node-right t) 0)))
        acc))))

(define min-depth 4)

(define (bench n)
  (let ([max-depth (max (+ min-depth 2) n)])
    (let ([stretch-depth (+ max-depth 1)])
      (printf "stretch tree of depth ~a\t check: ~a\n"
              stretch-depth
              (check (make 0 stretch-depth))))
    (let ([long-lived-tree (make 0 max-depth)])
      (for ([d (in-range 4 (+ max-depth 1) 2)])
        (let ([iterations (expt 2 (+ (- max-depth d) min-depth))])
          (printf "~a\t trees of depth ~a\t check: ~a\n"
                  (* 2 iterations)
                  d
                  (for/fold ([c 0]) ([i (in-range iterations)])
                    (+ c (+ (check (make i d))
                                (check (make (- 0 i) d))))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))

(time-run bench)
