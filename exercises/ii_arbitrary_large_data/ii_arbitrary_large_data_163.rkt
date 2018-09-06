;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 163.

; List-of-numbers -> List-of-numbers
; converts each given temperature in Fahrenheit to celsius
; measurement

(check-expect (convertFC '()) '())
(check-expect
 (convertFC (cons 100 (cons 80 '())))
 (cons  (convert 100) (cons  (convert 80) '())))

(define (convertFC tmprs)
  (cond
    [(empty? tmprs) '()]
    [else
     (cons (convert (first tmprs)) (convertFC (rest tmprs)))]))

; Number -> Number
; computes the wage for h hours of work
(define (convert t)
 (* (- t 32) 5/9))