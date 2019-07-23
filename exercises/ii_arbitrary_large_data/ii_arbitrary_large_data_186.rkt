;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_186) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 186.

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
 
(check-satisfied (sort> '()) sorted>?)
(check-satisfied (sort> (list 3 2 1)) sorted>?)

(check-satisfied (sort> (list 1 2 3)) sorted>?)

(check-satisfied (sort> (list 12 20 -5)) sorted>?)

(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; List-of-numbers -> Boolean
; produces #true if numbers are in descending order
; otherwise, returns #false

;; tests
(check-expect (sorted>? '()) #true)

(check-expect
 (sorted>? (cons 4 (cons 3 (cons 2 '()))))
 #true)

(check-expect
 (sorted>? (cons 4 (cons 5 (cons 2 '()))))
 #false)

(define (sorted>? nel)
  (cond
    [(empty? nel) #true]
    [(cons? nel)
     (if (empty? (rest nel)) #true
         (and (> (first nel) (first (rest nel)))
              (sorted>? (rest nel))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-expect (sort>/bad (list 1 3 4)) (list 4 3 1))

(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

; Q. Can you formulate a test case that shows that sort>/bad
; is not a sorting function? Can you use check-satisfied to
; formulate this test case?

; A. No, because the answer to (sorted>? l) will always be #true