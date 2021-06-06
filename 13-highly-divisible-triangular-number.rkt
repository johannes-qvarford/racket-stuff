#lang racket

;; Thi is a mess: the simple calculation of triangle number denominator length
;; was not efficient enough, so I tried, and failed to find a more efficient
;; method usings primes, but it all feels very complicated.

(require "common/test.rkt")
(require threading)
(require math/number-theory)

(define (divisible-by? denom numer)
  (= 0 (remainder numer denom)))
(test divisible-by?
      ['(1 1) #true]
      ['(2 1) #true]
      ['(2 2) #true]
      ['(3 2) #false])

(define (divisibles n)
  (filter (curry divisible-by? n) (range 1 (add1 n))))
(test divisibles
      ['(1) '(1)]
      ['(2) '(1 2)]
      ['(3) '(1 3)]
      ['(4) '(1 2 4)])

(define primes
  (stream-filter prime? (in-naturals 2)))

(define (prime-factors n)
  (let iter ([left n] [remaining-primes primes])
    (let ([f (stream-first remaining-primes)]
          [r (stream-rest remaining-primes)])
    (cond
      [(< left f) '()]
      [(divisible-by? left f) (cons f (iter (/ left f) r))]
      [else (iter left r)]))))
(define s2l (λ (x) x))

(module+ test
  (require rackunit)
  (test-case "prime-factors"
    (check-equal? (s2l (prime-factors 2)) '(2))
    (check-equal? (s2l (prime-factors 3)) '(3))
    (check-equal? (s2l (prime-factors 8)) '(2))
    (check-equal? (s2l (prime-factors 12)) '(2 3))))

(define (divisibles# tri)
  (if (< tri 3) tri
      (let ([f (stream-length (prime-factors (triangle-number tri)))])
        (+ f (triangle-number (- f 2))))))

(define (triangle-number n)
  (* (/ n 2) (+ n 1)))
(test triangle-number
      ['(1) 1]
      ['(2) 3]
      ['(3) 6])

(define (triangle-numbers)
  (stream-map triangle-number (in-naturals 1)))

(define (triangle-number-with-over-x-divisibles x)
  (~> (in-naturals 2)
      (stream-ormap (λ (m) (and ((divisibles# m) . > . x) m)) _)
      triangle-number))

