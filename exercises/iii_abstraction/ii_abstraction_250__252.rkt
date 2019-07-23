;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_250__252) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 250.

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; [Number -> Number] Number -> [List-of Number]
; applies the given function R to each number between
; given number n and 0 (incl.) in a list

(define (tabulate R n)
  (cond
    [(= n 0) (list (R n))]
    [else
     (cons
       (R n)
       (tabulate R (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list

(define (sin-from-tabulate n)
  (tabulate sin n))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list

(define (sqrt-from-tabulate n)
  (tabulate sqrt n))

; Exercise 251.

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [Number -> Number] [List-of Number] -> Number
; computes the given function R to the numbers
; on l

(define (fold1 R l)
  (cond
    [(empty? (rest l)) (R (first l))]
    [else
     (R (first l)
        (fold1 R (rest l)))]))

; Exercise 252.

; [List-of Number] -> Number

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

; [ITEM ... -> ITEM] [List-of ITEM] Any -> ITEM
; produces an item by applying the given input function
; to each element of the input list. Returns
; given any input for empty lists

(define (fold2 R l v)
  (cond
    [(empty? l) v]
    [else
     (R (first l)
        (fold2 R (rest l) v))]))

(define (product2 l)
  (fold2 * l 1))

(define (image*2 l)
  (fold2 place-dot l emt))

(product2 (list 1 2 3))
(image*2 (list (make-posn 2 3) (make-posn 4 10)))