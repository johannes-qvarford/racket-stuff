(module 1-multiples-of-3-and-5 racket/base
  (#%module-begin
   (require rackunit)
   (define (range bottom top)
     (if (>= bottom top) '() (cons bottom (range (+ bottom 1) top))))
   (define (is-multiple-of? mult x) (eq? 0 (modulo x mult)))
   (define (are-some-multiples-of? mults x)
     (if (eq? '() mults)
       #f
       (or (is-multiple-of? (car mults) x)
           (are-some-multiples-of? (cdr mults) x))))
   (define (multiples-in xs)
     (filter (lambda (x) (are-some-multiples-of? '(3 5) x)) xs))
   (define (sum xs) (apply + xs))
   (module+
    test
    (test-case
     "small-range"
     (check-equal? (range 1 1) '())
     (check-equal? (range 1 2) '(1))
     (check-equal? (range 1 10) '(1 2 3 4 5 6 7 8 9)))
    (test-case
     "multiple multiples"
     (check-equal? (are-some-multiples-of? '() 101) #f)
     (check-equal? (are-some-multiples-of? '(3) 9) #t)
     (check-equal? (are-some-multiples-of? '(3) 10) #f)
     (check-equal? (are-some-multiples-of? '(3 5) 5) #t)
     (check-equal? (are-some-multiples-of? '(3 5) 3) #t)
     (check-equal? (are-some-multiples-of? '(3 5) 4) #f))
    (test-case
     "multiples"
     (check-equal? (is-multiple-of? 3 3) #t)
     (check-equal? (is-multiple-of? 3 9) #t)
     (check-equal? (is-multiple-of? 5 20) #t)
     (check-equal? (is-multiple-of? 5 21) #f))
    (test-case
     "multiples-in"
     (check-equal? (multiples-in (range 1 10)) '(3 5 6 9)))
    (test-case "sum" (check-equal? (sum (multiples-in (range 1 10))) 23))
    (test-case
     "answer"
     (check-equal? (sum (multiples-in (range 1 1000))) 233168)))))
