#lang racket/base

(require rackunit)

(require racket/list)

(define (ce a b) (check-equal? a b))

; Thinking of looking for the first prime factor until x is returned
(define (first-prime-factor x)
        (define (iter x xsqrt n)
                (cond [(> n xsqrt) x] [(eq? (modulo x n) 0) n] [(iter x xsqrt (+ n 1))]))
        (iter x (sqrt x) 2))

(ce (first-prime-factor 6) 2)

(ce (first-prime-factor 5) 5)

(ce (first-prime-factor 15) 3)

; definining prime-factors in terms of first-prime-factor works really nice.
(define (prime-factors x)
        (let ((first (first-prime-factor x)))
             (if (eq? first x) (list x) (cons first (prime-factors (/ x first))))))

(ce (prime-factors 6) '(2 3))

(ce (prime-factors 15) '(3 5))

(define big-factors (prime-factors 600851475143))

big-factors

(last big-factors)