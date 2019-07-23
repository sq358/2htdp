;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_263_266) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 263.

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

; (inf (list 2 1 3))

; Exercise 264.

; Nelon -> Number
; determines the biggest number on l
(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define biggest-in-rest (sup (rest l))))
       (if (> (first l) biggest-in-rest)
           (first l)
           biggest-in-rest))]))

; (sup (list 2 1 3))

; Exercise 265.

((local ((define (f x) (+ (* 4 (sqr x)) 3))) f)
 1)

; Exercise 266.

((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)

