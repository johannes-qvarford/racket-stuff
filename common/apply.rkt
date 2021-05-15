#lang racket/base

(define-syntax apply-to-successive-pairs
    (syntax-rules ()
        [(apply-to-successive-pairs p) (void)]
        [(apply-to-successive-pairs p a b c ...) (begin (p a b) (apply-to-successive-pairs p c ...))]
    )
)


(module+ test
    (require rackunit)
    (require racket/port)
    (define (double-printer a b) (print a) (print b))
    (define output1
        (with-output-to-string (lambda ()
            (apply-to-successive-pairs double-printer))))
    (check-equal? output1 "")
    (define output2
        (with-output-to-string (lambda ()
            (apply-to-successive-pairs double-printer 1 2))))
        (check-equal? output2 "12")
    (define output3
        (with-output-to-string (lambda ()
            (apply-to-successive-pairs double-printer 1 2 3 4))))
        (check-equal? output3 "1234")
)

(provide apply-to-successive-pairs)