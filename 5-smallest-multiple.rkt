#lang racket/base

(require "common/problem.rkt")

#|
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
|#

; Take all primes below 20.
; Then multiply each prime by itself repeatedly, stopping before each exceeds 20
; Then multiply the multiplied primes together.

(define 2-to-20 (in-range-list 2 21))

(define contributors (filter prime? 2-to-20))

(define (contribution max x)
    (define (iter current)
        (if (> current max) (/ current x)
        (iter (* current x))))
    (iter x)
)
(ce (contribution 20 2) 16)

(define contributions (map (curry contribution 20) contributors))

(foldl * 1 contributions)