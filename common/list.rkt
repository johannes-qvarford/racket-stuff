#lang racket/base

(define (some? p xs) (not (eq? #f (memf p xs))))

(provide some?)

(module+ test
         (require rackunit)
         (test-case "some?"
                    (check-false (some? even? '(1 3 5)))
                    (check-true (some? even? '(1 3 5 2)))))