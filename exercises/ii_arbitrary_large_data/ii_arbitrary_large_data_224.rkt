;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_224) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 224.

(require 2htdp/universe)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; List-of-missiles is one of:
; - '()
; - (cons Missle List-of-missiles)

(define-struct sigs.v3 [ufo tank missiles])
; A SIGS.v3 is a structure:
;   (make-sigs.v3 UFO Tank List-of-missiles)
; interpretation represents the complete state of a
; space invader game

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

(define PROXIMITY 15) ; in pixels

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

(define TEST-SIGSV3-NO-MISSILE
  (make-sigs.v3 TEST-UFO TEST-TANK '()))

(define TEST-SIGSV3-MISSILE
  (make-sigs.v3 TEST-UFO TEST-TANK (list TEST-MISSILE)))

(define TEST-SIGSV3-GAME-OVER-1
  (make-sigs.v3
   (make-posn 20 HEIGHT) TEST-TANK-GOES-RIGHT '()))

(define TEST-SIGSV3-GAME-OVER-2
  (make-sigs.v3
   (make-posn 70 30)
   (make-tank 70 3)
   (list (make-posn 70 (+ 30 (/ PROXIMITY 2))))))

; Big bang
(define (si-main.v3 s)
  (big-bang s
    [to-draw si-render.v3]
    [on-tick si-move.v3]
    [on-key si-control.v3]
    [stop-when si-game-over.v3? si-render-final.v3]
    [state #true]))

; SIGS.v3 -> Image
; renders the given game stage as image
(check-expect
 (si-render.v3 TEST-SIGSV3-NO-MISSILE)
 (tank-render.v3
  (sigs.v3-tank TEST-SIGSV3-NO-MISSILE)
  (ufo-render.v3
   (sigs.v3-ufo TEST-SIGSV3-NO-MISSILE)
   (missiles-render
    (sigs.v3-missiles TEST-SIGSV3-NO-MISSILE)
    BACKGROUND))))

(check-expect
 (si-render.v3 TEST-SIGSV3-MISSILE)
 (tank-render.v3
  (sigs.v3-tank TEST-SIGSV3-MISSILE)
  (ufo-render.v3
   (sigs.v3-ufo TEST-SIGSV3-MISSILE)
   (missiles-render
    (sigs.v3-missiles TEST-SIGSV3-MISSILE)
    BACKGROUND))))

(define (si-render.v3 s)
  (tank-render.v3
   (sigs.v3-tank s)
   (ufo-render.v3
    (sigs.v3-ufo s)
    (missiles-render
     (sigs.v3-missiles s)
     BACKGROUND))))

; Tank Image -> Image
; adds an image of tank t to scene s

;; tests
(check-expect
 (tank-render.v3 TEST-TANK BACKGROUND)
 (place-image
  TANK (tank-loc TEST-TANK) SKY-HEIGHT BACKGROUND))

(define (tank-render.v3 t s)
  (place-image
   TANK (tank-loc t) SKY-HEIGHT s))

; UFO Image -> Image
; adds an image of ufo u to the scene s

;; tests
(check-expect
 (ufo-render.v3 TEST-UFO BACKGROUND)
 (place-image
  UFO (posn-x TEST-UFO) (posn-y TEST-UFO) BACKGROUND))

(define (ufo-render.v3 u s)
  (place-image
   UFO (posn-x u) (posn-y u) s))

; List-of-missiles Image -> Image
; renders missiles to the scene s

(check-expect (missiles-render '() BACKGROUND) BACKGROUND)
(check-expect
 (missiles-render
  (list
   (make-posn 10 15)
   (make-posn 10 14)
   (make-posn 10 13))
 BACKGROUND)
 (place-image
  MISSILE
  10
  15
  (place-image
   MISSILE
   10
   14
   (place-image
    MISSILE
    10
    13
    BACKGROUND))))

(define (missiles-render m im)
  (cond
    [(empty? m) im]
    [else
     (place-image
      MISSILE
      (posn-x (first m))
      (posn-y (first m))
      (missiles-render (rest m) im))]))

; SIGS.v3 -> Boolean
; determines if the game is over.
; returns #true if UFO lands with proximy PROXIMITY
; to HEIGHT or one of the missiles hits the UFO with proximity
; PROXIMITY.

(check-expect
 (si-game-over.v3? TEST-SIGSV3-NO-MISSILE) #false)

(check-expect
 (si-game-over.v3? TEST-SIGSV3-MISSILE) #false)

(check-expect
 (si-game-over.v3? TEST-SIGSV3-GAME-OVER-1) #true)

(check-expect
 (si-game-over.v3? TEST-SIGSV3-GAME-OVER-2) #true)

(define (si-game-over.v3? s)
  (cond
    [(empty? (sigs.v3-missiles s))
     (> (posn-y (sigs.v3-ufo s)) (- HEIGHT PROXIMITY))]
    [(cons? (sigs.v3-missiles s))
     (or
      (any-missile-hit-ufo? s)
      (> (posn-y (sigs.v3-ufo s)) (- HEIGHT PROXIMITY)))]
    ))

; SIGS.v3 -> Image
; renders game scene after game over

;; tests
(check-expect
 (si-render-final.v3 TEST-SIGSV3-GAME-OVER-1)
 (place-image
  GAME-OVER (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(check-expect
 (si-render-final.v3 TEST-SIGSV3-GAME-OVER-2)
 (place-image
  PLAYER-WON (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(define (si-render-final.v3 s)
  (place-image
   (cond
     [(empty? (sigs.v3-missiles s)) GAME-OVER]
     [else
      (if (any-missile-hit-ufo? s) PLAYER-WON GAME-OVER)])
   (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

; SIGS.v3 -> Boolean
; determines if any of the missiles hit the ufo

(check-expect (any-missile-hit-ufo? TEST-SIGSV3-NO-MISSILE) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV3-MISSILE) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV3-GAME-OVER-1) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV3-GAME-OVER-2) #true)

(define (any-missile-hit-ufo? s)
  (cond
    [(empty? (sigs.v3-missiles s)) #false]
    [else
     (or
      (missile-hit-ufo? (first (sigs.v3-missiles s)) (sigs.v3-ufo s))
      (if (empty? (rest (sigs.v3-missiles s)))
          #false
          (any-missile-hit-ufo?
           (make-sigs.v3
            (sigs.v3-ufo s)
            (sigs.v3-tank s)
            (rest (sigs.v3-missiles s))))))]))

(define (missile-hit-ufo? m u)
  (and
   (<= (abs (- (posn-y m) (posn-y u))) PROXIMITY)
   (<= (abs (- (posn-x m) (posn-x u))) PROXIMITY)))

; SIGS.v3 -> SIGS.v3
; for every clock tick creates the next space-invader

;; tests
(check-random
 (si-move.v3
  (make-sigs.v3 (make-posn 10 40) (make-tank 30 2) '()))
  (si-move-proper.v3
   (make-sigs.v3 (make-posn 10 40) (make-tank 30 2) '())
   (ufo-random-jump (make-posn 10 40)))
  )

(check-random
 (si-move.v3
  (make-sigs.v3
   (make-posn 10 40) (make-tank 30 2) (list (make-posn 20 40))))
  (si-move-proper.v3
   (make-sigs.v3
    (make-posn 10 40) (make-tank 30 2) (list (make-posn 20 40)))
   (ufo-random-jump (make-posn 10 40)))
  )

(define (si-move.v3 s)
  (si-move-proper.v3 s (ufo-random-jump (sigs.v3-ufo s))))

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
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 40 3) '()) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) '()))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 40 -3) '()) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) '()))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 10 3) '()) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) '()))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 10 -3) '()) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) '()))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 40 3) (list (make-posn 35 60))) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 40 -3) (list (make-posn 35 60))) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 10 3) (list (make-posn 35 60))) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v3
  (make-sigs.v3
   (make-posn 10 12) (make-tank 10 -3) (list (make-posn 35 60))) 2)
 (make-sigs.v3
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) (list (make-posn 35 54))
  ))

(define (si-move-proper.v3 s delta)
  (make-sigs.v3
   (make-posn
    (+ (posn-x (sigs.v3-ufo s)) delta)
    (+ (posn-y (sigs.v3-ufo s)) UFO-SPEED))
   (if (tank-is-out? (sigs.v3-tank s))
       (make-tank
        (- (tank-loc (sigs.v3-tank s)) (tank-vel (sigs.v3-tank s)))
        (tank-vel (sigs.v3-tank s)))
       (move-tank (sigs.v3-tank s)))
   (move-missiles (sigs.v3-missiles s))))

; List-of-missiles -> List-of-missiles
; for every clock tick, moves missiles with missiles speed

(check-expect (move-missiles '()) '())
(check-expect
 (move-missiles (list (make-posn 35 60) (make-posn 35 70)))
 (list (make-posn 35 54) (make-posn 35 64)))

(define (move-missiles m)
  (cond
    [(empty? m) '()]
    [else
     (cons
      (make-posn
       (posn-x (first m))
       (- (posn-y (first m)) MISSILE-SPEED))
      (if (empty? (rest m))
          '()
          (move-missiles (rest m))))]))

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

; SIGS.v3 KeyEvent -> SIGS.v3
; based on key event ke producses a new space-invader state
; pressing the left arrow ensures that the tank moves left;
; pressing the right arrow ensures that the tank moves right; and
; pressing the space bar fires the missile if it hasn’t been launched yet.

(check-expect
 (si-control.v3
  (make-sigs.v3
    (make-posn 10 12) (make-tank 10 3) '()) "left")
 (make-sigs.v3
  (make-posn 10 12) (make-tank 10 -3) '()))

(check-expect
 (si-control.v3
  (make-sigs.v3
    (make-posn 10 12) (make-tank 10 -3) '()) "right")
 (make-sigs.v3
  (make-posn 10 12) (make-tank 10 3) '()))

(check-expect
 (si-control.v3
  (make-sigs.v3
    (make-posn 10 12) (make-tank 10 -3) '()) " ")
 (make-sigs.v3
  (make-posn 10 12) (make-tank 10 -3)
  (list (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT)))
  ))

(check-expect
 (si-control.v3
  (make-sigs.v3
    (make-posn 10 12)
    (make-tank 10 -3)
    (list (make-posn 10 (- (- SKY-HEIGHT TANK-HEIGHT) 2)))) " ")
 (make-sigs.v3
  (make-posn 10 12)
  (make-tank 10 -3)
  (list
   (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT))
   (make-posn 10 (- (- SKY-HEIGHT TANK-HEIGHT) 2)))))

(check-expect
 (si-control.v3
  (make-sigs.v3
    (make-posn 10 12) (make-tank 10 -3) '()) "u")
 (make-sigs.v3
  (make-posn 10 12) (make-tank 10 -3) '()))

(define (si-control.v3 s k)
  (cond
    [(or (string=? k "left") (string=? k "right"))
     (make-sigs.v3
      (sigs.v3-ufo s)
      (next-tank (sigs.v3-tank s) k)
      (sigs.v3-missiles s))]
    [(string=? k " ")
     (make-sigs.v3
      (sigs.v3-ufo s)
      (sigs.v3-tank s)
      (cons
       (make-posn
        (tank-loc (sigs.v3-tank s)) (- SKY-HEIGHT TANK-HEIGHT))
       (sigs.v3-missiles s)))]
    [else s]))

; Number KeyEvent -> Number
; returns next tank based on KeyEvent ke

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "left") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "right") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "left") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "right") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "h") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "y") TEST-TANK-GOES-LEFT)

(define (next-tank t k)
  (cond
    [(string=? k "left")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (tank-vel t) (* -1 (tank-vel t))))]
    [(string=? k "right")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (* -1 (tank-vel t)) (tank-vel t)))]
    [else t]))

(si-main.v3
  (make-sigs.v3   (make-posn (/ WIDTH 2) 10) (make-tank (/ WIDTH 2) 3) '()))