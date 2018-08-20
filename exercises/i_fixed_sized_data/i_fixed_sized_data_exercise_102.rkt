;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_102) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exerc(ise 102.

(require 2htdp/universe)

(define-struct sigs.v2 [ufo tank missileornot])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location


; constants
(define SKY-HEIGHT 450)
(define GROUND-HEIGHT 30)

(define HEIGHT
  (+ SKY-HEIGHT GROUND-HEIGHT))

(define WIDTH 500)

(define BACKGROUND
  (above
   (rectangle WIDTH SKY-HEIGHT "solid" "black")
   (rectangle WIDTH GROUND-HEIGHT "solid" "brown")))

(define TANK-SPEED 3)
(define UFO-SPEED 2)
(define MISSILE-SPEED (* UFO-SPEED 3))

(define TANK-WIDTH 35)
(define TANK-HEIGHT 15)
(define MISSILE-SIZE 9)
(define UFO-BASE-WIDTH 30)
(define UFO-BASE-HEIGHT 15)

(define PROXIMITY 5) ; in pixels

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

(define GAME-OVER
  (text "Game over!" 11 "red"))

(define PLAYER-WON
  (text "Player won!" 11 "blue"))

(define TEST-TANK-GOES-RIGHT (make-tank 10 3))
(define TEST-TANK-GOES-LEFT (make-tank 10 -3))

(define TEST-UFO (make-posn 50 30))
(define TEST-TANK TEST-TANK-GOES-RIGHT)
(define TEST-MISSILE (make-posn 10 50))

(define TEST-SIGSV2-NO-MISSILE
  (make-sigs.v2 TEST-UFO TEST-TANK #false))

(define TEST-SIGSV2-MISSILE
  (make-sigs.v2 TEST-UFO TEST-TANK TEST-MISSILE))

(define TEST-SIGSV2-GAME-OVER-1
  (make-sigs.v2
   (make-posn 20 HEIGHT) TEST-TANK-GOES-RIGHT #false))

(define TEST-SIGSV2-GAME-OVER-2
  (make-sigs.v2
   (make-posn 70 30)
   (make-tank 70 3)
   (make-posn 70 (+ 30 (/ PROXIMITY 2)))))

; Big bang
(define (si-main.v2 s)
  (big-bang s
    [to-draw si-render.v2]
    [on-tick si-move.v2]
    [on-key si-control.v2]
    [stop-when si-game-over.v2? si-render-final.v2]))

; SIGS.v2 -> Image
; renders the given game stage as image
(check-expect
 (si-render.v2 TEST-SIGSV2-NO-MISSILE)
 (tank-render.v2
  (sigs.v2-tank TEST-SIGSV2-NO-MISSILE)
  (ufo-render.v2
   (sigs.v2-ufo TEST-SIGSV2-NO-MISSILE)
   (missile-render.v2
    (sigs.v2-missileornot TEST-SIGSV2-NO-MISSILE)
    BACKGROUND))))

(check-expect
 (si-render.v2 TEST-SIGSV2-MISSILE)
 (tank-render.v2
  (sigs.v2-tank TEST-SIGSV2-MISSILE)
  (ufo-render.v2
   (sigs.v2-ufo TEST-SIGSV2-MISSILE)
   (missile-render.v2
    (sigs.v2-missileornot TEST-SIGSV2-MISSILE)
    BACKGROUND))))

(define (si-render.v2 s)
  (tank-render.v2
   (sigs.v2-tank s)
   (ufo-render.v2
    (sigs.v2-ufo s)
    (missile-render.v2
     (sigs.v2-missileornot s)
     BACKGROUND))))

; Tank Image -> Image
; adds an image of tank t to scene s

;; tests
(check-expect
 (tank-render.v2 TEST-TANK BACKGROUND)
 (place-image
  TANK (tank-loc TEST-TANK) SKY-HEIGHT BACKGROUND))

(define (tank-render.v2 t s)
  (place-image
   TANK (tank-loc t) SKY-HEIGHT s))

; UFO Image -> Image
; add an image of ufo u to scene s

;; tests
(check-expect
 (ufo-render.v2 TEST-UFO BACKGROUND)
 (place-image
  UFO (posn-x TEST-UFO) (posn-y TEST-UFO) BACKGROUND))

(define (ufo-render.v2 u s)
  (place-image
   UFO (posn-x u) (posn-y u) s))

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s
 
;; test
(check-expect
 (missile-render.v2 #false BACKGROUND) BACKGROUND)

(check-expect
 (missile-render.v2 (make-posn 10 30) BACKGROUND)
 (place-image MISSILE 10 30 BACKGROUND))

(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
      (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 -> Boolean
; determines if the game is over.
; returns #true if UFO lands with proximy PROXIMITY
; to HEIGHT or the missile hits the UFO with proximity
; PROXIMITY.

(check-expect
 (si-game-over.v2? TEST-SIGSV2-NO-MISSILE) #false)

(check-expect
 (si-game-over.v2? TEST-SIGSV2-MISSILE) #false)

(check-expect
 (si-game-over.v2? TEST-SIGSV2-GAME-OVER-1) #true)

(check-expect
 (si-game-over.v2? TEST-SIGSV2-GAME-OVER-2) #true)

(define (si-game-over.v2? s)
  (cond
    [(boolean? (sigs.v2-missileornot s))
     (> (posn-y (sigs.v2-ufo s)) (- HEIGHT PROXIMITY))]
    [(posn? (sigs.v2-missileornot s))
     (or
      (and
       (<
        (abs (- (posn-y (sigs.v2-missileornot s))
            (posn-y (sigs.v2-ufo s)))) PROXIMITY)
       (<
        (abs (- (posn-x (sigs.v2-missileornot s))
                (posn-x (sigs.v2-ufo s)))) PROXIMITY))
      (> (posn-y (sigs.v2-ufo s)) (- HEIGHT PROXIMITY)))]
    ))

; SIGS.v2 -> Image
; renders game scene after game over

;; tests
(check-expect
 (si-render-final.v2 TEST-SIGSV2-GAME-OVER-1)
 (place-image
  GAME-OVER (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(check-expect
 (si-render-final.v2 TEST-SIGSV2-GAME-OVER-2)
 (place-image
  PLAYER-WON (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(define (si-render-final.v2 s)
  (place-image
   (cond
     [(boolean? (sigs.v2-missileornot s)) GAME-OVER]
     [(posn? (sigs.v2-missileornot s))
      (if (missile-hit-ufo.v2? s) PLAYER-WON GAME-OVER)])
   (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

; SIGS.v2 -> Boolean
; determines if a missile has hit a ufo

;; tests
(check-expect
 (missile-hit-ufo.v2? TEST-SIGSV2-GAME-OVER-2)
 #true)

(check-expect
 (missile-hit-ufo.v2? TEST-SIGSV2-GAME-OVER-1)
 #false)

(check-expect
 (missile-hit-ufo.v2? TEST-SIGSV2-MISSILE)
 #false)

(check-expect
 (missile-hit-ufo.v2? TEST-SIGSV2-NO-MISSILE)
 #false)

(define (missile-hit-ufo.v2? s)
  (cond
    [(boolean? (sigs.v2-missileornot s)) #false]
    [(posn? (sigs.v2-missileornot s))
     (and
      (<
       (abs (- (posn-y (sigs.v2-missileornot s))
               (posn-y (sigs.v2-ufo s)))) PROXIMITY)
      (<
       (abs (- (posn-x (sigs.v2-missileornot s))
               (posn-x (sigs.v2-ufo s)))) PROXIMITY))
     ]))

; SIGS.v2 -> SIGS.v2
; for every clock tick creates the next space-invader

;; tests
(check-random
 (si-move.v2
  (make-sigs.v2 (make-posn 10 40) (make-tank 30 2) #false))
  (si-move-proper.v2
   (make-sigs.v2 (make-posn 10 40) (make-tank 30 2) #false)
   (ufo-random-jump (make-posn 10 40)))
  )

(check-random
 (si-move.v2
  (make-sigs.v2
   (make-posn 10 40) (make-tank 30 2) (make-posn 20 40)))
  (si-move-proper.v2
   (make-sigs.v2
    (make-posn 10 40) (make-tank 30 2) (make-posn 20 40))
   (ufo-random-jump (make-posn 10 40)))
  )

(define (si-move.v2 s)
  (si-move-proper.v2 s (ufo-random-jump (sigs.v2-ufo s))))

; UFO -> Number
; for input u returns n where n can be negative
; or positive, and (abs n) is within [0, WIDTH) value

(check-random
 (ufo-random-jump (make-posn 5 12))
 (* -1 (random 3)))

(check-random
 (ufo-random-jump (make-posn 12 20))
 (random 3))

(define (ufo-random-jump u)
 (cond
   [(= (modulo (posn-x u) 2) 0) (random 3)]
   [else (* -1 (random 3))]))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta

;; tests
(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 40 3) #false) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) #false))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 40 -3) #false) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) #false))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 10 3) #false) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) #false))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 10 -3) #false) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) #false))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 40 3) (make-posn 35 60)) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) (make-posn 35 54)
  ))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 40 -3) (make-posn 35 60)) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) (make-posn 35 54)
  ))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 10 3) (make-posn 35 60)) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) (make-posn 35 54)
  ))

(check-expect
 (si-move-proper.v2
  (make-sigs.v2
   (make-posn 10 12) (make-tank 10 -3) (make-posn 35 60)) 2)
 (make-sigs.v2
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) (make-posn 35 54)
  ))

(define (si-move-proper.v2 s delta)
  (make-sigs.v2
   (make-posn
    (+ (posn-x (sigs.v2-ufo s)) delta)
    (+ (posn-y (sigs.v2-ufo s)) UFO-SPEED))
   (if (tank-is-out? (sigs.v2-tank s))
       (make-tank
        (- (tank-loc (sigs.v2-tank s)) (tank-vel (sigs.v2-tank s)))
        (tank-vel (sigs.v2-tank s)))
       (move-tank (sigs.v2-tank s)))
   (cond
     [(boolean? (sigs.v2-missileornot s)) #false]
     [(posn? (sigs.v2-missileornot s))
      (make-posn
       (posn-x (sigs.v2-missileornot s))
       (- (posn-y (sigs.v2-missileornot s)) MISSILE-SPEED))]
     )))

(define (tank-is-out? t)
  (or
   (>=
    (tank-loc (move-tank t)) (- WIDTH (/ (image-width TANK) 2)))
   (<=
    (tank-loc (move-tank t)) (/ (image-width TANK) 2))))

; Tank -> Tank
; moves tank t based on tank-vel
;; tests
(check-expect
 (move-tank (make-tank 10 -3)) (make-tank 7 -3))
(check-expect
 (move-tank (make-tank 10 3)) (make-tank 13 3))

(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t)))

; SIGS.v2 KeyEvent -> SIGS.v2
; based on key event ke producses a new space-invader state
; pressing the left arrow (j) ensures that the tank moves left;
; pressing the right arrow (k) ensures that the tank moves right; and
; pressing the space bar fires the missile if it hasn’t been launched yet.

(check-expect
 (si-control.v2
  (make-sigs.v2
    (make-posn 10 12) (make-tank 10 3) #false) "j")
 (make-sigs.v2
  (make-posn 10 12) (make-tank 10 -3) #false))

(check-expect
 (si-control.v2
  (make-sigs.v2
    (make-posn 10 12) (make-tank 10 -3) #false) "k")
 (make-sigs.v2
  (make-posn 10 12) (make-tank 10 3) #false))

(check-expect
 (si-control.v2
  (make-sigs.v2
    (make-posn 10 12) (make-tank 10 -3) #false) " ")
 (make-sigs.v2
  (make-posn 10 12) (make-tank 10 -3)
  (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT))
  ))

(check-expect
 (si-control.v2
  (make-sigs.v2
    (make-posn 10 12) (make-tank 10 -3) #false) "u")
 (make-sigs.v2
  (make-posn 10 12) (make-tank 10 -3) #false))

(check-expect
 (si-control.v2
  (make-sigs.v2
    (make-posn 10 12) (make-tank 10 -3) (make-posn 10 40)) " ")
 (make-sigs.v2
  (make-posn 10 12) (make-tank 10 -3) (make-posn 10 40)
  ))

(define (si-control.v2 s k)
  (cond
    [(or (string=? k "j") (string=? k "k"))
     (make-sigs.v2
      (sigs.v2-ufo s)
      (next-tank (sigs.v2-tank s) k)
      (sigs.v2-missileornot s))]
    [(string=? k " ")
        (make-sigs.v2
         (sigs.v2-ufo s)
         (sigs.v2-tank s)
         (cond
           [(boolean? (sigs.v2-missileornot s))
            (make-posn
             (tank-loc (sigs.v2-tank s)) (- SKY-HEIGHT TANK-HEIGHT))]
           [(posn? (sigs.v2-missileornot s)) (sigs.v2-missileornot s)])
         )]
       [else s]))

; Number KeyEvent -> Number
; returns velocity v based on KeyEven ke

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "j") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "k") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "j") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "k") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "h") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "y") TEST-TANK-GOES-LEFT)

(define (next-tank t k)
  (cond
    [(string=? k "j")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (tank-vel t) (* -1 (tank-vel t))))]
    [(string=? k "k")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (* -1 (tank-vel t)) (tank-vel t)))]
    [else t]))

(si-main.v2
 (make-sigs.v2
  (make-posn (/ WIDTH 2) 10) (make-tank (/ WIDTH 2) 3) #false))