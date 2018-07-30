;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_97) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 97.

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

(define SKY-HEIGHT 250)
(define GROUND-HEIGHT 30)

(define HEIGHT
  (+ SKY-HEIGHT GROUND-HEIGHT))

(define WIDTH 350)

(define BACKGROUND
  (above
   (rectangle WIDTH SKY-HEIGHT "solid" "black")
   (rectangle WIDTH GROUND-HEIGHT "solid" "brown")))

(define TANK-WIDTH 35)
(define TANK-HEIGHT 15)
(define MISSILE-SIZE 3)
(define UFO-BASE-WIDTH 30)
(define UFO-BASE-HEIGHT 15)

(define TANK-COLOR "green")
(define MISSILE-COLOR "red")
(define UFO-BASE-COLOR "orange")
(define UFO-CAP-COLOR "purple")

(define TANK
  (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
  
(define MISSILE
  (triangle MISSILE-SIZE "solid" MISSILE-COLOR))

(define UFO-BASE
  (rectangle
   UFO-BASE-WIDTH UFO-BASE-HEIGHT "solid" UFO-BASE-COLOR))
(define UFO-CAP
  (ellipse
   UFO-BASE-HEIGHT UFO-BASE-WIDTH "solid" UFO-CAP-COLOR))
(define UFO
  (overlay UFO-CAP UFO-BASE))

(define INITIAL-TANK-SCN
   (place-image
   TANK
   (/ WIDTH 2)
   SKY-HEIGHT
   BACKGROUND))
    

(define (aim? scn) scn)

(define (fired? scn) scn)

; Tank Image -> Image 
; adds t to the given image im
;; tests
(check-expect
 (tank-render (make-tank 10 3) BACKGROUND)
 (place-image TANK 10 HEIGHT BACKGROUND))

(define (tank-render t im)
  (place-image TANK (tank-loc t) HEIGHT im))
 
; UFO Image -> Image 
; adds u to the given image im
;; tests
(check-expect
 (ufo-render (make-posn 50 60) BACKGROUND)
 (place-image UFO 50 60 BACKGROUND))

(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image
; adds m to the given image im
(check-expect
 (missile-render (make-posn 40 100) BACKGROUND)
 (place-image MISSILE 40 100 BACKGROUND))

(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

;; Q. When do the following two expressions produce the same result?
;(tank-render
;  (fired-tank s)
;  (ufo-render (fired-ufo s)
;              (missile-render (fired-missile s)
;                              BACKGROUND)))

;(ufo-render
;  (fired-ufo s)
;  (tank-render (fired-tank s)
;               (missile-render (fired-missile s)
;                               BACKGROUND)))
;; A. When s is equal in both