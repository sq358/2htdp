;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_100) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 100.

(require 2htdp/universe)

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

(define PROXIMITY 3) ; in pixels

(define TANK-COLOR "green")
(define MISSILE-COLOR "red")
(define UFO-BASE-COLOR "orange")
(define UFO-CAP-COLOR "purple")

(define TEST-TANK-GOES-RIGHT (make-tank 10 3))
(define TEST-TANK-GOES-LEFT (make-tank 10 -3))
(define TEST-AIM
  (make-aim (make-posn (/ WIDTH 2) 1) (make-tank 30 2)))
(define TEST-FIRED
  (make-fired (make-posn (/ WIDTH 2) 40)
              (make-tank 10 3)
              (make-posn 10 40)))

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


(define (si-main s)
  (big-bang s
    [to-draw si-render]
    [on-key si-control]
    [on-tick si-move]
    [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; renders the given game state on top of BACKGROUND

(check-expect
 (si-render TEST-AIM)
 (tank-render (aim-tank TEST-AIM)
              (ufo-render
               (aim-ufo TEST-AIM)
               BACKGROUND)))

(check-expect
 (si-render TEST-FIRED)
 (tank-render (fired-tank TEST-FIRED)
              (ufo-render
               (fired-ufo TEST-FIRED)
               (missile-render
                (fired-missile TEST-FIRED) BACKGROUND))))

(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKGROUND)))]))

; Tank Image -> Image
; adds t to the given image im
;; tests
(check-expect
 (tank-render TEST-TANK-GOES-RIGHT BACKGROUND)
 (place-image TANK (tank-loc TEST-TANK-GOES-RIGHT) HEIGHT BACKGROUND))

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
                TEST-TANK-GOES-LEFT))
              #false)

(check-expect (si-game-over?
               (make-aim
                (make-posn 20 278)
                TEST-TANK-GOES-RIGHT))
              #false)

(check-expect (si-game-over?
               (make-aim
                (make-posn 20 HEIGHT)
                TEST-TANK-GOES-RIGHT))
              #true)

(check-expect (si-game-over?
               (make-fired
                (make-posn 20 15)
                TEST-TANK-GOES-RIGHT
                (make-posn 10 40)))
              #false)

(check-expect (si-game-over?
               (make-fired
                (make-posn 20 38)
                TEST-TANK-GOES-RIGHT
                (make-posn (+ 21 PROXIMITY) 40)))
              #false)


(check-expect (si-game-over?
               (make-fired
                (make-posn 20 38)
                TEST-TANK-GOES-RIGHT
                (make-posn 18 40)))
              #true)

(check-expect (si-game-over?
               (make-fired
                (make-posn 20 39)
                TEST-TANK-GOES-RIGHT
                (make-posn 20 40)))
              #true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (> (posn-y (aim-ufo s)) (- HEIGHT PROXIMITY))]
    [(fired? s)
     (or
      (and
       (<
        (abs (- (posn-y (fired-missile s)) (posn-y (fired-ufo s)))) PROXIMITY)
       (<
        (abs (- (posn-x (fired-missile s)) (posn-x (fired-ufo s)))) PROXIMITY))
      (> (posn-y (fired-ufo s)) (- HEIGHT PROXIMITY)))]))


; SIGS -> Image
; renders game scene after game over
(check-expect (si-render-final
               (make-aim
                (make-posn 10 40)
                (make-tank 30 2)))
              (place-image
               GAME-OVER
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))

(check-expect (si-render-final
               (make-fired
                (make-posn 10 40)
                (make-tank 30 2)
                (make-posn 10 41)))
              (place-image
               PLAYER-WON
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))

(define (si-render-final s)
              (place-image
               (cond
                 [(aim? s) GAME-OVER]
                 [(fired? s)
                  (if (missile-hit-ufo? s) PLAYER-WON GAME-OVER)])
               (/ WIDTH 2)
               (/ HEIGHT 2)
               BACKGROUND))


; SIGS -> Boolean
; determines if a missile has hit a ufo in fired stage

(check-expect
 (missile-hit-ufo?
  (make-fired (make-posn 5 39) TEST-TANK-GOES-RIGHT (make-posn 20 40)))
 #false)

(check-expect
 (missile-hit-ufo?
  (make-fired (make-posn 20 39) TEST-TANK-GOES-RIGHT (make-posn 20 40)))
 #true)

(check-expect
 (missile-hit-ufo? TEST-AIM) #false)

(define (missile-hit-ufo? s)
  (cond
    [(fired? s)
     (and
      (<
       (abs (- (posn-y (fired-missile s)) (posn-y (fired-ufo s)))) PROXIMITY)
      (<
       (abs (- (posn-x (fired-missile s)) (posn-x (fired-ufo s)))) PROXIMITY))]
    [(aim? s) #false]))

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

; SIGS Number -> SIGS
; moves the space-invader objects predictably by delta
(check-expect
 (si-move-proper
  (make-aim (make-posn 10 12) (make-tank 10 3)) 2)
 (make-aim
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3)))

(check-expect
 (si-move-proper
  (make-aim (make-posn 10 12) (make-tank 50 3)) 2)
 (make-aim
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 53 3)))

(check-expect
 (si-move-proper
  (make-aim (make-posn 50 12) (make-tank 10 -3)) -1)
 (make-aim (make-posn 49 (+ 12 UFO-SPEED)) (make-tank 13 -3)))

(check-expect
 (si-move-proper
  (make-aim
   (make-posn 10 12)
   (make-tank
    (- WIDTH (/ (image-width TANK) 2)) 3)) 2)
 (make-aim
  (make-posn 12 (+ 12 UFO-SPEED))
  (make-tank (- (- WIDTH (/ (image-width TANK) 2)) 3) 3)))

(check-expect
 (si-move-proper
  (make-fired
   (make-posn 50 12) (make-tank 10 -3) (make-posn 10 12)) -1)
 (make-fired
  (make-posn 49 (+ 12 UFO-SPEED))
  (make-tank 13 -3)
  (make-posn 10 (- 12 MISSILE-SPEED))))

(define (si-move-proper w delta)
 (cond
   [(aim? w)
    (make-aim
     (make-posn
      (+ (posn-x (aim-ufo w)) delta)
      (+ (posn-y (aim-ufo w)) UFO-SPEED))
     (if (tank-is-out? (aim-tank w))
         (make-tank
          (- (tank-loc (aim-tank w)) (tank-vel (aim-tank w)))
          (tank-vel (aim-tank w)))
         (move-tank (aim-tank w)))
     )]
   [(fired? w)
    (make-fired
     (make-posn
      (+ (posn-x (fired-ufo w)) delta)
      (+ (posn-y (fired-ufo w)) UFO-SPEED))
     (if (tank-is-out? (fired-tank w))
         (make-tank
          (- (tank-loc (fired-tank w)) (tank-vel (fired-tank w)))
          (tank-vel (fired-tank w)))
         (move-tank (fired-tank w)))
     (make-posn
      (posn-x (fired-missile w))
      (- (posn-y (fired-missile w)) MISSILE-SPEED))
     )]
   ))

(define (tank-is-out? t)
  (or
   (>= (tank-loc (move-tank t)) (- WIDTH (/ (image-width TANK) 2)))
   (<= (tank-loc (move-tank t)) (/ (image-width TANK) 2))))

; Tank -> Tank
; moves tank t based on tank-vel
;; tests
(check-expect
 (move-tank (make-tank 10 -3)) (make-tank 7 -3))
(check-expect
 (move-tank (make-tank 10 3)) (make-tank 13 3))

(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t)))

; SIGS KeyEvent -> SIGS
; based on key event ke producses a new space-invader state
; pressing the left arrow (j) ensures that the tank moves left;
; pressing the right arrow (k) ensures that the tank moves right; and
; pressing the space bar fires the missile if it hasn’t been launched yet.

;; tests
(check-expect
 (si-control
  (make-aim (make-posn 10 12) (make-tank 10 3)) "j")
 (make-aim (make-posn 10 12) (make-tank 10 -3)))

(check-expect
 (si-control
  (make-aim (make-posn 10 12) (make-tank 10 -3)) "k")
 (make-aim (make-posn 10 12) (make-tank 10 3)))

(check-expect
 (si-control
  (make-aim (make-posn 10 12) (make-tank 10 -3)) " ")
   (make-fired
   (make-posn 10 12)
   (make-tank 10 -3)
   (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT))))

(check-expect
 (si-control
  (make-aim (make-posn 10 12) (make-tank 10 -3)) "u")
  (make-aim (make-posn 10 12) (make-tank 10 -3)))

(check-expect
 (si-control
  (make-fired
   (make-posn 10 12)
   (make-tank 10 -3)
   (make-posn 10 10)) " ")
   (make-fired
   (make-posn 10 12)
   (make-tank 10 -3)
   (make-posn 10 10)))

(check-expect
 (si-control
  (make-fired
   (make-posn 10 12)
   (make-tank 10 -3)
   (make-posn 10 10)) "j")
   (make-fired
   (make-posn 10 12)
   (next-tank (make-tank 10 -3) "j")
   (make-posn 10 10)))

(define (si-control s k)
  (cond
    [(aim? s)
     (cond
       [(or (string=? k "j") (string=? k "k"))
            (make-aim (aim-ufo s) (next-tank (aim-tank s) k))]
       [(string=? k " ")
        (make-fired
         (aim-ufo s)
         (aim-tank s)
         (make-posn
          (tank-loc (aim-tank s))
          (- SKY-HEIGHT TANK-HEIGHT)))]
       [else s])]
    [(fired? s)
     (cond
       [(or (string=? k "j") (string=? k "k"))
            (make-fired
             (fired-ufo s)
             (next-tank (fired-tank s) k)
             (fired-missile s))]
       [else s])]))

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
