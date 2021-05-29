#lang racket

(require math/base)
(require (only-in srfi/41 stream-take-while))
(require stream-etc)
(require "common/test.rkt")

; It felt incorrect to hard-code when to stop producing fibs, so I produced an infinite stream instead.
; I also wanted to play with streams overall.
(define (fibs-stream)
  (define (fibs-rec a b) (stream-cons (+ a b) (fibs-rec b (+ a b))))
  (fibs-rec 0 1))
(module+ test
  (require rackunit)
  (test-case "fibs-stream"
    (check-equal? 
      (stream->list (stream-take-while (curry > 10) (fibs-stream)))
      '(1 2 3 5 8))))

(answer
  (~>>  (fibs-stream)
        (stream-take-while (lambda (x) (< x 4000000)))
        (stream-filter even?)
        stream-sum)
  4613732)