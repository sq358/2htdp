;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_114) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 114.

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

(define-struct vcat [location happiness])
; VCat is a structure:
;  (define-struct vcat [Number Number])
; interpretation (make-vcat x h) is a cat
;  at x-coordinate with the happiness score
;  h

(define-struct vcham [location color happiness])
; VCham is a structure:
;  (define-struct vcham [Number String Number])
; interpretation (make-vcham x "blue" h) is a blue
; chameleon at x-coordinate with the happiness score

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

(define (sigs? s)
  (cond
    [(or (aim? s) (fired? s)) #true]
    [else #false]))

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

(define (coordinate? n)
  (cond
    [(and (number? n) (not (= n 0))) #true]
    [(posn? n) #true]
    [else #false]))

(define (vanimal? a)
  (cond
    [(or (vcat? a) (vcham? a)) #true]
    [else #false]))