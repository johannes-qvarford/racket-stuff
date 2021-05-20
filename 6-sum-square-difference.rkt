#lang racket/base

(require (only-in algorithms sum))

(require racket/list)

(require racket/math)

(define (sum-of-squares n) (sum (map sqr (range 1 (+ n 1)))))

(define (square-of-sum n) (sqr (* (+ 1 n) (/ n 2))))

(define (sum-square-difference n)
        (define sum-of-s (sum-of-squares n))
        (define s-of-sum (square-of-sum n))
        (- s-of-sum sum-of-s))

(module+ test
         (require rackunit)
         (test-case "sum-of-squares"
                    (check-equal? (sum-of-squares 1) 1)
                    (check-equal? (sum-of-squares 3) 14)
                    (check-equal? (sum-of-squares 10) 385))
         (test-case "square-of-sum"
                    (check-equal? (square-of-sum 1) 1)
                    (check-equal? (square-of-sum 3) 36)
                    (check-equal? (square-of-sum 4) 100)
                    (check-equal? (square-of-sum 10) 3025))
         (test-case "sum-square-difference"
                    (check-equal? (sum-square-difference 1) 0)
                    (check-equal? (sum-square-difference 3) 22)
                    (check-equal? (sum-square-difference 10) 2640))
         (test-case "answer" (check-equal? (sum-square-difference 100) 25164150)))