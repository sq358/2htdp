;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ii_abstraction_235__238) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 235

(define (contains-atom? l)
  (contains? "atom" l))

(define (contains-basic? l)
  (contains? "basic" l))

(define (contains-zoo? l)
  (contains? "zoo" l))

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Exercise 236.

; Lon -> Lon
; adds 1 to each item on l

(check-expect (add1* '()) '())
(check-expect (add1* '(1 2 3)) '(2 3 4))

(define (add1* l)
  (plus 1 l))

; Lon -> Lon
; adds 5 to each item on l

(check-expect (plus5 '()) '())
(check-expect (plus5 '(1 2 3)) '(6 7 8))

(define (plus5 l)
  (plus 5 l))

; Number Lon -> Lon
; adds number n to each item in the given list of numbers

(check-expect (plus 5 '()) '())
(check-expect (plus 1 '(1 2 3)) '(2 3 4))

(define (plus n l)
  (cond
    [(empty? l) l]
    [else
     (cons (+ n (first l))
           (plus n (rest l)))]))

; Exercise 237.

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 5 10)

; Exercise 238.

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l) (inf (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l) (sup (rest l)))]))

(define list1
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))

(define list2
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))

; determines the smallest 
; number on l

; following tests are commented out because as expected
; they're too slow

;(check-expect (inf-1 list1) 1)
;(check-expect (inf-1 list2) 1)

(define (inf-1 l)
  (compare < l))

; Nelon -> Number
; determines the largest 
; number on l

; following tests are commented out because as expected
; they're too slow

;(check-expect (sup-1 list1) 1)
;(check-expect (sup-1 list2) 1)

(define (sup-1 l)
  (compare > l))

; determines the smallest 
; number on l

(check-expect (inf-2 list1) 1)
(check-expect (inf-2 list2) 1)

(define (inf-2 l)
  (compare min l))

; Nelon -> Number
; determines the largest 
; number on l

(check-expect (sup-2 list1) 25)
(check-expect (sup-2 list2) 25)

(define (sup-2 l)
  (compare max l))

; Function Nelon -> Number
; produces the number in non-empty lon based on the
; input comparator

(define (compare R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (R (first l) (compare R (rest l)))]))