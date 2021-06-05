#lang racket
(require "common/test.rkt")
(require threading)

#;#(#(1 2 3) #(4 5 6) #(7 8 9))

; adj-prod a x y n -> max of
; adj-right a x y n
; adj-down a x y n
; adj-diag a x y n

(define (valid-index? a i)
  (and (< i (vector-length a)) (>= i 0)))
(test valid-index?
      [(list #(1 1) 0) #t]
      [(list #(1 1) 1) #t]
      ['(#(1 1) 2) #f]
      ['(#(1 1) -1) #f])

(define (inside? a p)
  (if (not (valid-index? a (cdr p)))
      #f
      (let ([row (vector-ref a (cdr p))])
        (valid-index? row (car p)))))
            
(define 3x3 (make-vector 3 (make-vector 3 2)))
(test inside?
      [(list 3x3 '(0 . 0)) #t]
      [(list 3x3 '(2 . 2)) #t]
      [(list 3x3 '(-1 . 0)) #f]
      [(list 3x3 '(0 . -1)) #f])

(define (map-range n f)
  (map f (range n)))

(define (right-position p n)
  (cons (+ (car p) n) (cdr p)))
(test right-position
      ['((0 . 0) 1) '(1 . 0)]
      ['((0 . 0) 2) '(2 . 0)]
      ['((1 . 2) 2) '(3 . 2)])

(define (position-value a p)
  (if (inside? a p)
      (vector-ref (vector-ref a (cdr p)) (car p))
      1))
(test position-value
      [(list 3x3 '(0 . 0)) 2]
      [(list 3x3 '(-1 . 0)) 1])

(define (adj-seq-value a p n f)
  (~>> (map-range n (curry f p))
       (map (curry position-value a) _)
       (apply * _)))

#;(define (adj-right-value a p n)
    (adj-seq-value a p n right-position))
(define adj-right-value (curryr adj-seq-value right-position))
(test adj-right-value
      [(list 3x3 '(0 . 0) 1) 2]
      [(list 3x3 '(0 . 0) 2) 4]
      [(list 3x3 '(0 . 0) 4) 8]
      [(list 3x3 '(-1 . 0) 2) 2]
      [(list 3x3 '(0 . -1) 2) 1]
      [(list 3x3 '(2 . 0) 2) 2])

(define (down-position p n)
  (cons (car p) (+ (cdr p) n)))
(test down-position
      [(list (cons 0 0) 0) (cons 0 0)]
      [(list (cons 0 1) 1) (cons 0 2)]
      [(list (cons 2 2) 2) (cons 2 4)])

(define adj-down-value (curryr adj-seq-value down-position))

(define (down-left-position p n)
  (cons (- (car p) n) (+ (cdr p) n)))
(test down-left-position
      [(list (cons 0 0) 0) (cons 0 0)]
      [(list (cons 0 0) 3) (cons -3 3)])

(define adj-down-left-value (curryr adj-seq-value down-left-position))

(define (down-right-position p n)
  (~> p (right-position _ n) (down-position _ n)))
(test down-right-position
      [(list (cons 0 0) 0) (cons 0 0)]
      [(list (cons 1 2) 3) (cons 4 5)])

(define adj-down-right-value (curryr adj-seq-value down-right-position))

(define (max-adj-value a p n)
  (~>> (list adj-down-left-value adj-right-value adj-down-value adj-down-right-value)
       (map (λ (f) (f a p n)) _)
       (apply max _)))

(define 3x3-1-to-9 #(#(1 2 3) #(4 5 6) #(7 8 9)))
(test max-adj-value
      [(list 3x3-1-to-9 (cons 0 0) 1) 1]
      [(list 3x3-1-to-9 (cons 0 0) 3) 45]
      [(list 3x3-1-to-9 (cons 2 0) 3) 162]
      [(list 3x3-1-to-9 (cons 0 2) 3) 504])

(define (row-positions a n)
  (map-range (vector-length (vector-ref a n)) (curry right-position (cons 0 n))))
(test row-positions
      [(list 3x3 0) '((0 . 0) (1 . 0) (2 . 0))]
      [(list 3x3 2) '((0 . 2) (1 . 2) (2 . 2))])

(define (flatten-1 l)
  (foldr append '() l))
(test flatten-1
      [(list '((1) (2) (3))) '(1 2 3)])

(define (matrix-positions a)
  (~> (vector-length a)
      (map-range _ (curry row-positions a))
      flatten-1))
(test matrix-positions
      [(list #(#(1))) '((0 . 0))]
      [(list 3x3) '((0 . 0) (1 . 0) (2 . 0) (0 . 1) (1 . 1) (2 . 1) (0 . 2) (1 . 2) (2 . 2))])

(define (max-matrix a n)
  (~> (matrix-positions a)
      (map (λ (p) (max-adj-value a p n)) _)
      (apply max _)))
(define (debug a n)
  (~> (matrix-positions a)
      (map (λ (p) (cons p (max-adj-value a p n))) _)
      (foldr (λ (p1 p2) (if (> (cdr p1) (cdr p2)) p1 p2)) (cons (cons -1 -1) -1) _)))

(test max-matrix
      [(list 3x3 1) 2]
      [(list 3x3 3) 8]
      [(list 3x3-1-to-9 1) 9]
      [(list 3x3-1-to-9 3) 504])

(define 3x3-1-to-9-string "01 02 03\n04 05 06\n07 08 09")
(define (read-vector s)
  (~> s
      (string-split _ " ")
      (map open-input-string _)
      (map read _)
      list->vector))
(test read-vector
      ['("01") #(1)]
      ['("01 02") #(1 2)])

(define (read-matrix s)
  (~> s
      (string-split _ "\n")
      (map read-vector _)
      list->vector))
(test read-matrix
      ['("01 02\n03 04") #(#(1 2) #(3 4))]
      ['("01\n02\n") #(#(1) #(2))])

(define matrix-string "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")
(define matrix (read-matrix matrix-string))

(answer
 (max-matrix matrix 4)
 70600674)
