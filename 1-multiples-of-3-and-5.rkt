#lang racket

(require math/base)
(require "common/list.rkt")
(require "common/test.rkt")

(define (is-multiple-of? mult x) (eq? 0 (modulo x mult)))
(test is-multiple-of?
  ['(3 3) #t]
  ['(3 9) #t]
  ['(5 20) #t]
  ['(5 21) #f])

(define (are-some-multiples-of? mults x)
  (some? (curryr is-multiple-of? x) mults))
(test are-some-multiples-of?
  ['(() 101) #f]
  ['((3) 9) #t]
  ['((3) 10) #f]
  ['((3 5) 5) #t]
  ['((3 5) 3) #t]
  ['((3 5) 4) #f])

(define (3-and-5-multiples-in xs)
  (filter (lambda (x) (are-some-multiples-of? '(3 5) x)) xs))
(test 3-and-5-multiples-in
  [(list (range 1 10)) '(3 5 6 9)])

(answer (sum (3-and-5-multiples-in (range 1 1000))) 233168)