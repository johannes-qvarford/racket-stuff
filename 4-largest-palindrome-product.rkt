#lang racket/base

; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

; Find the largest palindrome made from the product of two 3-digit numbers.

; The number has to be less than 1 000 000
; The number has to be greater or equal to 10 000

; palindrome?
; could do a easy string solution that stringifies the number and compares it against the reverse-order string.
; harder would be to split into list of digits i.e. 135 -> '(5 3 1)
; reverse it, and make sure that the new list is equal to the first.

(require racket/list)

(require racket/stream)

(require racket/function)

(require "common/list.rkt")

(define (digits x)
        (let ((q (quotient x 10)) (m (modulo x 10)))
             (if (= q 0) (list m) (cons m (digits q)))))

; I like this implementation - it's very straightforward.
(define (palindrome? x) (let ((d (digits x))) (equal? d (reverse d))))

; You could probably generate the stream of palindromes by knowing the bounds and the properties of palindromes.
; e.g. among 123xxx, there is only one palindrome, 123321.
; Linear search should be fine for this scale.

; This and the following 2 procedures were originally part of the same iterative procedure.
; I should probably have realized earlier that the problem was too big for one procedure to tackle.
(define (qoutient-is-3-digit-number? x n)
        (and (= (modulo x n) 0) (>= (/ x n) 100) (< (/ x n) 1000)))

(define 100-to-999 (range 100 999))

(define (divisible-by-2-3-digit-numbers? n)
        (some? (curry qoutient-is-3-digit-number? n) 100-to-999))

(module+ test
         (require rackunit)
         (test-case "digits"
                    (check-equal? (digits 1) '(1))
                    (check-equal? (digits 123) '(3 2 1)))
         (test-case "palindrome?"
                    (check-equal? (palindrome? 1) #t)
                    (check-equal? (palindrome? 22) #t)
                    (check-equal? (palindrome? 404) #t)
                    (check-equal? (palindrome? 4005) #f))
         (test-case "divisible-by-2-3-digit-numbers?"
                    (check-equal? (divisible-by-2-3-digit-numbers? 12000) #t)
                    (check-equal? (divisible-by-2-3-digit-numbers? 12001) #f)
                    (check-equal? (divisible-by-2-3-digit-numbers? 121) #f)
                    (check-equal? (divisible-by-2-3-digit-numbers? 999999) #f))
         (test-case "answer"
                    (define palindromes-less-than-1mil
                        (stream-filter palindrome? (in-range 1000000 1 -1)))
                    (define divs
                            (stream-filter divisible-by-2-3-digit-numbers? palindromes-less-than-1mil))
                    (check-equal? (stream-first divs) 906609)))