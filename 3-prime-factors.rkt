#lang racket

(require "common/test.rkt")
(require threading)

; Thinking of looking for the first prime factor until x is returned
(define (smallest-prime-factor x)
  (define (iter x xsqrt n)
    (cond 
      [(> n xsqrt) x]
      [(eq? (modulo x n) 0) n]
      [(iter x xsqrt (+ n 1))]))
  (iter x (sqrt x) 2))
(test smallest-prime-factor
  ['(6) 2]
  ['(5) 5]
  ['(15) 3])

; definining prime-factors in terms of smallest-prime-factor works really nice.
(define (prime-factors x)
  (let ([first (smallest-prime-factor x)])
    (if (eq? first x)
        (list x)
        (cons first (prime-factors (/ x first))))))
(test prime-factors
  ['(6) '(2 3)]
  ['(15) '(3 5)])

(answer (~> (prime-factors 600851475143) last) 6857)