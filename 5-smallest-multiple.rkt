#lang racket/base

(require racket/list)

(require racket/function)

(require (only-in math/number-theory prime?))

(require (only-in algorithms product))

; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

; Take all primes below 20.
; Then multiply each prime by itself repeatedly, stopping before each exceeds 20
; Then multiply the multiplied primes together.

(define (contribution max x)
    (define (iter current)
        (if (> current max) (/ current x)
        (iter (* current x))))
    (iter x)
)

(module+ test
    (require rackunit)
    (test-case "contribution"
        (check-equal? (contribution 20 2) 16)
    )
    (test-case "answer"
        (define 2-to-20 (range 2 21))
        (define contributors (filter prime? 2-to-20))
        (define contributions (map (curry contribution 20) contributors))
        (check-equal? (product contributions) 232792560)
    )
)