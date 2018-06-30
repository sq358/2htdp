; Exercise 39.
(define WIDTH-Of-WORLD 200)

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

(define CAR-BODY
  (above/align "center"
               (rectangle 15 WHEEL-DIAMETER "solid" "red")
               (rectangle (* 8 WHEEL-RADIUS)
                          (* 2 WHEEL-RADIUS) "solid" "red")))
(define CAR
  (underlay/offset
   CAR-BODY
   0
   WHEEL-DIAMETER
   BOTH-WHEELS))
