;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_95) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 95.

; A SIGS is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a
; space invader game


(make-aim (make-posn 20 10) (make-tank 28 -3))
; Corresponds to the following state:
;   (make-aim UFO Tank)
; UFO is in position (20, 10) of the canvas
; and Tank is in the position (20, HEIGHT)
; with the speed of -3 which means it's
; moving 3 pixels per clock tick towards left.

(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))

; Corresponds to the following state:
;   (make-fired UFO Tank Missile)
; UFO is in position (20, 10) of the canvas,
; Tank is in the position (20, HEIGHT)
; and is moving 3 pixels per clock tick towards
; left while missle is fired and is just emerging
; from the Tank.

(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))

; Corresponds to the following state:
;   (make-fired UFO Tank Missile)
; UFO is in position (20, 100) of the canvas,
; Tank is in the position (100, HEIGHT)
; and is moving 3 pixels per clock tick towards
; right while missle is collided with UFO
