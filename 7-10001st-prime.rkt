#lang racket/base

(require math/number-theory)

(module+ test
         (require rackunit)
         (test-case "answer"
                    (define (zero-based n) (- n 1))
                    (check-equal? (nth-prime (zero-based 6)) 13)
                    (check-equal? (nth-prime (zero-based 10001)) 104743)))