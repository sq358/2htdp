;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_101) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exerc(ise 101.

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game

(define SKY-HEIGHT 450)
(define GROUND-HEIGHT 30)

(define HEIGHT
  (+ SKY-HEIGHT GROUND-HEIGHT))

(define WIDTH 500)

(define BACKGROUND
  (above
   (rectangle WIDTH SKY-HEIGHT "solid" "black")
   (rectangle WIDTH GROUND-HEIGHT "solid" "brown")))

(define MISSILE-SIZE 9)
(define MISSILE-COLOR "red")

(define MISSILE
  (triangle MISSILE-SIZE "solid" MISSILE-COLOR))

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s 

(check-expect
 (missile-render.v2 #false BACKGROUND) BACKGROUND)

(check-expect
 (missile-render.v2 (make-posn 10 30) BACKGROUND)
 (place-image MISSILE 10 30 BACKGROUND))

(define (missile-render.v2 m im)
  (if (false? m) im
      (place-image MISSILE (posn-x m) (posn-y m) im)))
                      

