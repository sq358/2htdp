;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_279__281) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 279.

; legal lambda expressions

(lambda (x y) (x y y))

(lambda () 10)

(lambda (x) x)

(lambda (x y) x)

; illegal lambda expressions

(lambda () 10)

(lambda x 10)

; Exercise 280.

((lambda (x y) (+ x (* x y))) 1 2) ; Result: 3

((lambda (x y)
   (+ x (local ((define z (* y y)))
          (+ (* 3 z) (/ 1 x)))))
 1 2) ; Result: 14

((lambda (x y)
   (+ x
      ((lambda (z)
         (+ (* 3 z) (/ 1 z)))
       (* y y))))
 1 2) ; Result: 13.25

; Exercise 281.

(lambda (x) (< x 10))

(lambda (x y) (number->string (* x y)))

(lambda (n) (if (= (/ n 2) 0) "even" "odd"))

(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

(lambda (i1 i2) (<= (ir-price i1) (ir-price i2)))

(define RED-DOT
  (circle 10 "red"))

(lambda (p img) (place-image (posn-x p) (posn-y p) RED-DOT img))

