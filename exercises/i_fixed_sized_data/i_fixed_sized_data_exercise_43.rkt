; Exercise 43.
(require 2htdp/universe)

; An AnimationState is a Number.
; interpretation the number of clock ticks
; since the animation started

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 30)

(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND
  (place-image TREE (- WIDTH-OF-WORLD 10)
               (- HEIGHT-OF-WORLD 10)
               (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

(define WHEEL-RADIUS 5)

(define WHEEL-DIAMETER
  (* WHEEL-RADIUS 2))

(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR-WIDTH 15)

(define CAR-BODY
  (above/align "center"
               (rectangle CAR-WIDTH WHEEL-DIAMETER "solid" "red")
               (rectangle (* 8 WHEEL-RADIUS)
                          (* 2 WHEEL-RADIUS) "solid" "red")))
(define CAR
  (underlay/offset
   CAR-BODY
   0
   WHEEL-DIAMETER
   BOTH-WHEELS))

(define Y-CAR (- HEIGHT-OF-WORLD 12))

; AnimationState -> AnimationState
; add 3 to the number of clock ticks since the
; start of the program.

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(define (tock as)
  (+ as 3))

; AnimationState -> Image
; places the car into BACKGROUND according to the
; animation state.

(check-expect (render 50)
              (place-image CAR 50 (+ Y-CAR (sin 50)) BACKGROUND))
(check-expect (render 100)
              (place-image CAR 100 (+ Y-CAR (sin 100)) BACKGROUND))
(check-expect (render 150)
              (place-image CAR 150 (+ Y-CAR (sin 150)) BACKGROUND))
(check-expect (render 200)
              (place-image CAR 200 (+ Y-CAR (sin 200)) BACKGROUND))

(define (render as)
  (place-image CAR as (+ Y-CAR (sin as)) BACKGROUND))

(define (stop as)
  (if (>= as (+ WIDTH-OF-WORLD (image-width CAR))) #true #false))

(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when stop]))
