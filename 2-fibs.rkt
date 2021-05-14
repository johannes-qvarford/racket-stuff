#lang racket/base

(require rackunit)

(require racket/stream)

; Made this version first, but remembered that we don't want x amount of fib numbers.
(define (fibs n)
        (define (iter index xs)
                (if (> index n)
                    xs
                    (let* ((prev1 (car xs)) (prev2 (cadr xs)) (new-prev1 (+ prev1 prev2)))
                          (iter (+ index 1) (cons new-prev1 xs)))))
        (reverse (iter 3 '(2 1))))

(test-case "fibs"
           (check-equal? (fibs 3) '(1 2 3))
           (check-equal? (fibs 4) '(1 2 3 5))
           (check-equal? (fibs 5) '(1 2 3 5 8)))

; It felt incorrect to hard-code when to stop producing fibs, so I produced an infinite stream instead.
; I also wanted to play with streams overall.
(define (fibs-stream)
        (define (fibs-rec a b) (stream-cons (+ a b) (fibs-rec b (+ a b))))
        (fibs-rec 0 1))

(define (stream-nth n s)
        (if (eq? n 1) (stream-first s) (stream-nth (- n 1) (stream-rest s))))

; I'm surprised that this isn't in a standard racket procedure
(define (stream-take-until p s)
        (cond [(stream-empty? s) '()]
              [(p (stream-first s)) '()]
              [else (stream-cons (stream-first s) (stream-take-until p (stream-rest s)))]))

(module+ test
        
         (define (ce a b) (check-equal? a b))
         (test-case "stream-first" (ce (stream-first (fibs-stream)) 1))
         (test-case "fibs"
                    (ce (stream->list (stream-take-until (lambda (x) (> x 10)) (fibs-stream)))
                        '(1 2 3 5 8)))
         (test-case "stream-nth"
                    (ce (stream-nth 1 (fibs-stream)) 1)
                    (ce (stream-nth 2 (fibs-stream)) 2)
                    (ce (stream-nth 3 (fibs-stream)) 3)
                    (ce (stream-nth 4 (fibs-stream)) 5))
         (test-case "answer"
                    (define (too-big? x) (> x 4000000))
                    (define big (stream-take-until too-big? (fibs-stream)))
                    (stream->list big)
                    (define big-even (stream-filter even? big))
                    (stream->list big-even)
                    (define big-even-sum (stream-fold + 0 big-even))
                    (ce big-even-sum 4613732)))