#lang racket/base

(require racket/stream)

(provide
 fibs)

(define (fibs)
  (stream*
   1
   1
   (let ([s (fibs)])
     (for/stream ([x (in-stream s)]
                  [y (in-stream (stream-rest s))])
       (+ x y)))))

(module+ test
  (require rackunit)
  (check-equal? (stream->list (stream-take (fibs) 8))
                '(1 1 2 3 5 8 13 21)))