#lang racket/base

(provide number->digit-list)

(define (number->digit-list x)
        (let ((q (quotient x 10)) (m (modulo x 10)))
             (if (= q 0) (list m) (cons m (number->digit-list q)))))

(module+ test
         (require rackunit)
         (check-equal? (number->digit-list 123) '(3 2 1)))