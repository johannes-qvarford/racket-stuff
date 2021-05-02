#lang racket/base

; This approach to the problem is more complicated and inefficient tthan it needs to be.

(require "common/problem.rkt")



; Creating the helper library really reduces the clutter.
; There were a lot of helper producedures needed for this single producure
; but it can now be expressed more in terms of the problem.
(define (smallest-factor n)
    (stream-find (curry no-remainder? n) (stream-tail (positive-natural-numbers-stream) 2))
)
(ce (smallest-factor 2) 2)
(ce (smallest-factor 4) 2)
(ce (smallest-factor 3) 3)
(ce (smallest-factor 6) 2)
(ce (smallest-factor 8) 2)
(ce (smallest-factor 9) 3)

(define (smallest-factors n)
    (let ((f (smallest-factor n)))
        (if (= f n) (list n)
        (cons f (smallest-factors (/ n f))))))
(ce (smallest-factors 2) '(2))
(ce (smallest-factors 4) '(2 2))
(ce (smallest-factors 3) '(3))
(ce (smallest-factors 6) '(2 3))
(ce (smallest-factors 8) '(2 2 2))
(ce (smallest-factors 9) '(3 3))

(define (make-bag-entry item count)
    (list item count))
(define (bag-entry-item b) (car b))
(define (bag-entry-count b) (cadr b))

(define (empty?-or-first-not= a b)
    (or (empty? a) (not= b (car a))))

; We need to keep track of the count of each smallest factor per number in a bag
; For each factor, we can then take the factor pair among bags with the highest count, and form a new bag
; By multiplying the (pow factor count) of all pairs in the bag we get the smallest number evenly divisible by all numbers
(define (ordered-list->bag xs)
    (define (iter item items-left item-count)
        (cond
            [(empty?-or-first-not= items-left item) (cons (make-bag-entry item item-count) (ordered-list->bag items-left))]
            [else (iter item (cdr items-left) (plus1 item-count))]))
    (if (empty? xs) '()
        (iter (car xs) xs 0)))
(ce (ordered-list->bag '()) '())
(ce (ordered-list->bag '(2)) '((2 1)))
(ce (ordered-list->bag '(2 2 2)) '((2 3)))
(ce (ordered-list->bag '(2 2 2 3)) '((2 3) (3 1)))
(ce (ordered-list->bag '(2 2 3 5 5)) '((2 2) (3 1) (5 2)))

(define (max-bag-entry a b) (if (> (bag-entry-count a) (bag-entry-count b)) a b))

(define (union-bags-by-max-count a b)
    (define (union-non-empty-bags-by-max-count a b)
        (let* (
            (ae (car a))
            (aei (bag-entry-item ae))
            (aec (bag-entry-count ae))
            (be (car b))
            (bei (bag-entry-item be))
            (bec (bag-entry-count be)))
            (cond
                [(> aei bei) (union-non-empty-bags-by-max-count b a)]
                [(= aei bei)
                    (cons
                        (if (> aec bec) ae be)
                        (union-bags-by-max-count (cdr a) (cdr b)))]
                [else (cons ae (union-bags-by-max-count (cdr a) b))]
            ))
    )
    (match (list a b)
        [(list '() bb) bb]
        [(list aa '()) aa]
        [else (union-non-empty-bags-by-max-count a b)]
    ))
(ce (union-bags-by-max-count '() '()) '())
(ce (union-bags-by-max-count '((1 2)) '()) '((1 2)))
(ce (union-bags-by-max-count '() '((2 3))) '((2 3)))
(ce (union-bags-by-max-count '((1 1)) '((1 2))) '((1 2)))
(ce (union-bags-by-max-count '((1 1) (2 3)) '((2 4))) '((1 1) (2 4)))
(ce (union-bags-by-max-count '((2 4)) '((1 1) (2 3))) '((1 1) (2 4)))

(define one-to-twenty (stream-take (stream-tail (positive-natural-numbers-stream) 1) 20))
(stream->list one-to-twenty)
;(smallest-factors 20)
(define lists (map smallest-factors one-to-twenty))
lists