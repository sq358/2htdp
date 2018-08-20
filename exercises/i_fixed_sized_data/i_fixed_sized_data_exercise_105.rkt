;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_105) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exerc(ise 105.

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
;   example:
;    (= c -12) means (posn 0 12)
;    (= c -1) means 1 pixels from top
;
; – a PositiveNumber 
; interpretation on the x axis, distance from left
;   example:
;    (= c 40) means (posn 40 0)
;    (= c 5) means (posn 5 0)
; – a Posn
; interpretation an ordinary Cartesian point
;   example:
;    (posn? (make-posn 10 20)) means (posn 10 20)
;    (posn? (make-posn 0 0)) means (posn 0 0)
