;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_99) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 99.

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

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

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

(define TANK-SPEED 3)
(define UFO-SPEED 2)
(define MISSILE-SPEED (* UFO-SPEED 3))

(define TANK-WIDTH 35)
(define TANK-HEIGHT 15)
(define MISSILE-SIZE 3)
(define UFO-BASE-WIDTH 30)
(define UFO-BASE-HEIGHT 15)

(define PROXIMITY 2) ; in pixels

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

; SIGS -> Boolean
; determines if the game is over.
; returns #true if UFO lands with proximy PROXIMITY
; to HEIGHT or the missile hits the UFO with proximity
; PROXIMITY.
(check-expect (si-game-over?
               (make-aim
                (make-posn 30 40)
                (make-tank 40 -3)))
              #false)

(check-expect (si-game-over?
               (make-aim
                (make-posn 20 278)
                (make-tank 25 2)))
              #false)

(check-expect (si-game-over?
               (make-aim
                (make-posn 20 279)
                (make-tank 25 2)))
              #true)

(check-expect (si-game-over?
               (make-fired
                (make-posn 20 15)
                (make-tank 25 2)
                (make-posn 10 40)))
              #false)

(check-expect (si-game-over?
               (make-fired
                (make-posn 20 38)
                (make-tank 25 2)
                (make-posn 18 40)))
              #false)

(check-expect (si-game-over?
               (make-fired
                (make-posn 19 39)
                (make-tank 25 2)
                (make-posn 20 40)))
              #true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (> (posn-y (aim-ufo s)) (- HEIGHT PROXIMITY))]
    [(fired? s)
     (and
      (<
       (- (posn-x (fired-missile s))(posn-x (fired-ufo s))) PROXIMITY)
      (<
       (- (posn-y (fired-missile s)) (posn-y (fired-ufo s))) PROXIMITY))]
    ))

; SIGS -> Image
; redners the game scene after game over
(check-expect (si-render-final
               (make-aim
                (make-posn 10 40)
                (make-tank 30 2)))
              (place-image
               (text "Game over!" 11 "red")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))

(check-expect (si-render-final
               (make-fired
                (make-posn 10 40)
                (make-tank 30 2)
                (make-posn 20 15)))
              (place-image
               (text "Player won!" 11 "blue")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))

(define (si-render-final s)
              (place-image
               (cond
                 [(aim? s)
                  (text "Game over!" 11 "red")]
                 [else
                  (text "Player won!" 11 "blue")])
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))


; SIGS -> SIGS
; for every clock tick creates the next space-invader
; state.

(check-random
 (si-move
  (make-aim (make-posn 10 40) (make-tank 30 2)))
 (si-move-proper
  (make-aim
   (make-posn 10 40) (make-tank 30 2))
  (ufo-random-jump (make-posn 10 40))))

(check-random
 (si-move
  (make-fired
   (make-posn 10 40) (make-tank 30 2) (make-posn 30 45)))
 (si-move-proper
  (make-fired
   (make-posn 10 40) (make-tank 30 2) (make-posn 30 45))
  (ufo-random-jump (make-posn 10 40))))

(define (si-move w)
  (si-move-proper w (ufo-random-jump
                     (cond
                       [(aim? w) (aim-ufo w)]
                       [else (fired-ufo w)]))))

; UFO -> Number
; for input u returns n where n can be negative
; or positive, and (abs n) is within [0, (posn-x u)) value

(check-random
 (ufo-random-jump (make-posn 5 12))
 (* -1 (random 5)))

(check-random
 (ufo-random-jump (make-posn 12 20))
 (random 12))

(define (ufo-random-jump u)
 (cond
   [(= (modulo (posn-x u) 2) 0) (random (posn-x u))]
   [else (* -1 (random (posn-x u)))]))

; SIGS Number -> SIGS
; moves the space-invader objects predictably by delta
(check-expect
 (si-move-proper
  (make-aim (make-posn 10 12) (make-tank 10 3)) 2)
 (make-aim
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 3)))

(check-expect
 (si-move-proper
  (make-aim (make-posn 50 12) (make-tank 10 -3)) -1)
 (make-aim (make-posn 49 (+ 12 UFO-SPEED)) (make-tank 7 -3)))

(check-expect
 (si-move-proper
  (make-fired
   (make-posn 50 12) (make-tank 10 -3) (make-posn 10 12)) -1)
 (make-fired
  (make-posn 49 (+ 12 UFO-SPEED))
  (make-tank 7 -3)
  (make-posn 10 (- 12 MISSILE-SPEED))))

(define (si-move-proper w delta)
 (cond
   [(aim? w)
    (make-aim
     (make-posn
      (+ (posn-x (aim-ufo w)) delta)
      (+ (posn-y (aim-ufo w)) UFO-SPEED))
     (make-tank
      (+ (tank-loc (aim-tank w)) (tank-vel (aim-tank w)))
      (tank-vel (aim-tank w)))
     )]
   [(fired? w)
    (make-fired
     (make-posn
      (+ (posn-x (fired-ufo w)) delta)
      (+ (posn-y (fired-ufo w)) UFO-SPEED))
     (make-tank
      (+ (tank-loc (fired-tank w)) (tank-vel (fired-tank w)))
      (tank-vel (fired-tank w)))
     (make-posn
      (posn-x (fired-missile w))
      (- (posn-y (fired-missile w)) MISSILE-SPEED))
     )]
   ))