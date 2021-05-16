#lang racket/base

(require math/base)

(require racket/stream)

(require (only-in srfi/41 stream-take-while))

(require stream-etc)

(require racket/function)

; It felt incorrect to hard-code when to stop producing fibs, so I produced an infinite stream instead.
; I also wanted to play with streams overall.
(define (fibs-stream)
        (define (fibs-rec a b) (stream-cons (+ a b) (fibs-rec b (+ a b))))
        (fibs-rec 0 1))

(module+ test
         (require rackunit)
         (test-case "fibs-stream"
                    (check-equal? (stream->list (stream-take-while (curry > 10) (fibs-stream)))
                                  '(1 2 3 5 8)))
         (test-case "answer"
                    (define (too-big? x) (> x 4000000))
                    (define big (stream-take-while (compose1 not too-big?) (fibs-stream)))
                    (define big-even (stream-filter even? big))
                    (define big-even-sum (stream-sum big-even))
                    (check-equal? big-even-sum 4613732)))