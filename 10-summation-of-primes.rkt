#lang racket

(require "common/test.rkt" math/number-theory threading (only-in srfi/41 stream-take-while))

(define (->stream s) (stream-cons (stream-first s) (->stream (stream-rest s))))

(define (sum-of-primes-less-than m)
  (~>> (in-naturals)
       ->stream
       (stream-take-while (λ (n) (< n m)))
       (stream-filter prime?)
       (stream-fold + 0)))


(test sum-of-primes-less-than
      ['(10) 17])

(answer (sum-of-primes-less-than 2000000) 142913828922)
