#lang racket/base

(require racket/list)

(define (triplet? a b c)
  (and (< a b c) (= (expt c 2) (+ (expt a 2) (expt b 2)))))

(define (find-triplet-product sum)
  (define (make-c a b) (- sum a b))
  (define largest-a (quotient sum 3))
  (for*/first ([a (range 0 sum)]
         [b (range 1 sum)]
         #:when (triplet? a b (make-c a b)))
    (* a b (make-c a b))))
  
(module test)
