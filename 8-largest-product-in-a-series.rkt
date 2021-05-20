#lang racket/base

(require errortrace)

(require racket/port)

(require racket/string)

(require racket/list)

(require racket/function)

(require threading)

(define (sub-lists lst) (if (empty? lst) '() (cons lst (sub-lists (rest lst)))))

(define (length>= n lst) (>= (length lst) n))

(define (sub-lists-of-length lst n)
        (~> lst sub-lists (filter (curry length>= n) _) (map (curryr take n) _)))

(define (adjecents-with-greatest-product adjecent-length digit-list)
        (~> (sub-lists-of-length digit-list adjecent-length)
            (map (curry apply *) _)
            (apply max _)))

(module+ test
         (require rackunit)
         (require "common/number.rkt")
         ; ; The racket-pretty-printer unfortunately removes backslashes in strings.
         (define \n (make-string 1 #\newline))
         (define digits
                 (~> (with-input-from-file "8-largest-product-in-a-series.data" port->string)
                     (string-replace _ \n "")
                     string->number
                     number->digit-list))
         (test-case "sub-lists"
                    (check-equal? (sub-lists '()) '())
                    (check-equal? (sub-lists '(1)) '((1)))
                    (check-equal? (sub-lists '(2 1)) '((2 1) (1))))
         (test-case "sub-lists-of-length"
                    (check-equal? (sub-lists-of-length '(1 2 3 4) 1) '((1) (2) (3) (4)))
                    (check-equal? (sub-lists-of-length '(1 2 3 4) 2) '((1 2) (2 3) (3 4))))
         (test-case "adjecents-with-greatest-product"
                    (check-equal? (adjecents-with-greatest-product 2 '(1 2 3 4 5)) 20)
                    (check-equal? (adjecents-with-greatest-product 2 '(11 2 3 4 5)) 22)
                    (check-equal? (adjecents-with-greatest-product 4 digits) 5832))
         (test-case "answer"
                    (check-equal? (adjecents-with-greatest-product 13 digits) 23514624000)))