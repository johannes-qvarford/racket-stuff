#lang racket/base

(require rackunit)
(require racket/list)
(require racket/stream)
(require racket/match)
(require (except-in math/number-theory permutations))

(provide (all-from-out rackunit))
(provide (all-from-out racket/list))
(provide (all-from-out racket/stream))
(provide (all-from-out racket/match))
(provide (all-from-out math/number-theory))
(provide ce)
(provide ces)
(provide plus1)
(provide not=)
(provide no-remainder?)

(provide in-range-list)
(provide find)

(provide stream-find)
(provide stream-take-until)
(provide positive-natural-numbers-stream)
(provide non-zero-positive-natural-numbers-stream)
(provide curry)

(define (ce a b) (check-equal? a b))
(define (ct a) (check-true a))
(define (ces a b) (ce (stream->list a) b))
(define (plus1 n) (+ 1 n))
(define (not= a b) (not (= a b)))

(define (no-remainder? n d) (= (modulo n d) 0))
(ct (no-remainder? 4 2))

(define (positive-natural-numbers-stream)
    (define (recur n)
        (stream-cons n (recur (plus1 n))))
    (recur 0)
)
(ce (stream->list (stream-take (positive-natural-numbers-stream) 3)) '(0 1 2))
(define (non-zero-positive-natural-numbers-stream)
    (stream-tail (positive-natural-numbers-stream) 1))

(define (stream-take-until p s)
    (cond 
        [(stream-empty? s) '()]
        [(p (stream-first s)) '()]
        [else (stream-cons (stream-first s) (stream-take-until p (stream-rest s)))]
    ))
(ce (stream->list (stream-take-until (lambda (x) (> x 5)) (positive-natural-numbers-stream)) ) '(0 1 2 3 4 5))

(define (stream-find p s)
    (cond 
        [(stream-empty? s) '()]
        [(p (stream-first s)) (stream-first s)]
        [else (stream-find p (stream-rest s))]
    ))
(ce (stream-find (lambda (x) (= x 5)) (positive-natural-numbers-stream)) 5)

(define (in-range-list a b)
    (stream->list (in-range a b)))

(define (find p xs)
    (cond
        [(empty? xs) '()]
        [(p (car xs)) (car xs)]
        [else (find p (cdr xs))]
    ))
(ce (find (lambda (x) (= x 5)) '(1 2 3 4 5 6)) 5)


(define curry (lambda (f . xs)
    (lambda ys (apply f (foldr cons ys xs)))))
(ct ((curry no-remainder? 4) 2))
(define curry-back (lambda (f . xs)
    (lambda ys (apply f (foldr cons xs ys)))))
(ct ((curry-back no-remainder? 2) 4))