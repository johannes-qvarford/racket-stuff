#lang racket

(require (for-syntax racket/syntax rackunit))
(require rackunit)

(define-syntax (checks stx)
    (syntax-case stx ()
        [
            (_ NAME [ARGS EXPECTED] ...)
            #`(begin
                #,@(for/list 
                    (
                        [args-stx (syntax->list #'(ARGS ...))]
                        [expected-stx (syntax->list #'(EXPECTED ...))]
                        [clause-stx (cdr (syntax->list stx))])
                    (quasisyntax/loc clause-stx (check-equal? (apply NAME #,args-stx) #,expected-stx))))]))

(module+ test
    (checks +)
    (checks + ['(1 2) 3]))

(define-syntax (test stx)
    (syntax-case stx ()
        [
            (test2 NAME ARGS ...)
            #'(module+ test
                (require rackunit)
                (test-case (symbol->string (quote NAME)) (checks NAME ARGS ...)))]
    )
)


(test +)
(test + ['(1 2) 3])

(define-syntax (answer stx)
    (syntax-case stx ()
        [
            (answer ACTUAL EXPECTED)
            #`(module+ test
                (require rackunit)
                (test-case "answer" #,(quasisyntax/loc stx (check-equal? ACTUAL EXPECTED))))]))

(answer (+ 1 2 3) 6)

(provide test answer)