#lang racket/base

(require racket/list)

; Thinking of looking for the first prime factor until x is returned
(define (smallest-prime-factor x)
        (define (iter x xsqrt n)
                (cond [(> n xsqrt) x] [(eq? (modulo x n) 0) n] [(iter x xsqrt (+ n 1))]))
        (iter x (sqrt x) 2))

; definining prime-factors in terms of smallest-prime-factor works really nice.
(define (prime-factors x)
        (let ((first (smallest-prime-factor x)))
             (if (eq? first x) (list x) (cons first (prime-factors (/ x first))))))

(module+ test
         (require rackunit)
         (test-case "smallest-prime-factor"
                    (check-equal? (smallest-prime-factor 6) 2)
                    (check-equal? (smallest-prime-factor 5) 5)
                    (check-equal? (smallest-prime-factor 15) 3))
         (test-case "prime-factors"
                    (check-equal? (prime-factors 6) '(2 3))
                    (check-equal? (prime-factors 15) '(3 5)))
         (test-case "answer"
                    (define big-factors (prime-factors 600851475143))
                    (check-equal? (last big-factors) 6857)))