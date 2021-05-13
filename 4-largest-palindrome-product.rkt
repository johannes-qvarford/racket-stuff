#lang racket/base

; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

; Find the largest palindrome made from the product of two 3-digit numbers.

; The number has to be less than 1 000 000
; The number has to be greater or equal to 10 000

; palindrome?
; could do a easy string solution that stringifies the number and compares it against the reverse-order string.
; harder would be to split into list of digits i.e. 135 -> '(5 3 1)
; reverse it, and make sure that the new list is equal to the first.
(require rackunit)

(require racket/list)

(require racket/stream)

(define (ce a b) (check-equal? a b))

(define (digits x)
        (let ((q (quotient x 10)) (m (modulo x 10)))
             (if (= q 0) (list m) (cons m (digits q)))))

(ce (digits 1) '(1))

(ce (digits 123) '(3 2 1))

; I like this implementation - it's very straightforward.
(define (palindrome? x) (let ((d (digits x))) (equal? d (reverse d))))

(ce (palindrome? 1) #t)

(ce (palindrome? 22) #t)

(ce (palindrome? 404) #t)

(ce (palindrome? 4005) #f)

; You could probably generate the stream of palindromes by knowing the bounds and the properties of palindromes.
; e.g. among 123xxx, there is only one palindrome, 123321.
; Linear search should be fine for this scale.

(define palindromes-less-than-1mil
        (stream-filter palindrome? (in-range 1000000 1 -1)))

; This and the following 2 procedures were originally part of the same iterative procedure.
; I should probably have realized earlier that the problem was too big for one procedure to tackle.
(define (qoutient-is-3-digit-number? x n)
        (and (= (modulo x n) 0) (>= (/ x n) 100) (< (/ x n) 1000)))

(define (stream-contains-any? f xs) (not (stream-empty? (stream-filter f xs))))

(define (divisible-by-2-3-digit-numbers? x)
        (stream-contains-any? (lambda (n) (qoutient-is-3-digit-number? x n))
                              (in-range 100 1000)))

(ce (divisible-by-2-3-digit-numbers? 12000) #t)

(ce (divisible-by-2-3-digit-numbers? 12001) #f)

(ce (divisible-by-2-3-digit-numbers? 121) #f)

(ce (divisible-by-2-3-digit-numbers? 999999) #f)

(define divs
        (stream-filter divisible-by-2-3-digit-numbers? palindromes-less-than-1mil))

(stream-first divs)