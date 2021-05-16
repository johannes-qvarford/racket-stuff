#lang racket/base

(require math/base)
(require racket/list)
(require racket/function)
(require "common/list.rkt")

(define (is-multiple-of? mult x) (eq? 0 (modulo x mult)))

(define (are-some-multiples-of? mults x)
        (some? (curryr is-multiple-of? x) mults))

(define (3-and-5-multiples-in xs)
        (filter (lambda (x) (are-some-multiples-of? '(3 5) x)) xs))

(module+ test
         (require rackunit)
         (test-case "is-multiple-of?"
                    (check-equal? (is-multiple-of? 3 3) #t)
                    (check-equal? (is-multiple-of? 3 9) #t)
                    (check-equal? (is-multiple-of? 5 20) #t)
                    (check-equal? (is-multiple-of? 5 21) #f))
         (test-case "are-some-multiples-of?"
                    (check-equal? (are-some-multiples-of? '() 101) #f)
                    (check-equal? (are-some-multiples-of? '(3) 9) #t)
                    (check-equal? (are-some-multiples-of? '(3) 10) #f)
                    (check-equal? (are-some-multiples-of? '(3 5) 5) #t)
                    (check-equal? (are-some-multiples-of? '(3 5) 3) #t)
                    (check-equal? (are-some-multiples-of? '(3 5) 4) #f))
         (test-case "3-and-5-multiples-in" (check-equal? (3-and-5-multiples-in (range 1 10)) '(3 5 6 9)))
         (test-case "answer" (check-equal? (sum (3-and-5-multiples-in (range 1 1000))) 233168)))