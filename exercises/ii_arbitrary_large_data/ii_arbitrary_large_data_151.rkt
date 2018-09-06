;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_151) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 151.

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
   [(zero? n) pi]
   ((positive? n)
    (add1 (add-to-pi (sub1 n))))))

(check-within (add 3 4) (+ 3 4) 0.001)

; N N -> Number
; computes (+ n x) without using +

(define (add n x)
  (cond
   [(zero? n) x]
   ((positive? n)
    (add1 (add (sub1 n) x)))))

;; Q. Why does the skeleton use check-within?
;; Because PositiveNumber can also be inexact