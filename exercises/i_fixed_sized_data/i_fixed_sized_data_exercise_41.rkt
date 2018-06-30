; Exercise 41.
(require 2htdp/universe)

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

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(define (tock ws)
  (+ 3 ws))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state

(check-expect (render 50)
              (place-image CAR 50 Y-CAR BACKGROUND))
(check-expect (render 100)
              (place-image CAR 100 Y-CAR BACKGROUND))
(check-expect (render 150)
              (place-image CAR 150 Y-CAR BACKGROUND))
(check-expect (render 200)
              (place-image CAR 200 Y-CAR BACKGROUND))

(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

(define (stop ws)
  (if (>= ws (+ WIDTH-OF-WORLD (image-width CAR))) #true #false))

(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when stop]))
