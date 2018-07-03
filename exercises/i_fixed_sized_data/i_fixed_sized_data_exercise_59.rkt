; Exercise 59.
(require 2htdp/universe)

(define LIGHT-SIZE 10)
(define GAP 5)
(define HEIGHT
  (+
   (* LIGHT-SIZE 2)
   (* GAP 2)))
(define WIDTH
  (+
   (* LIGHT-SIZE 6)
   (* GAP 4)))

; TrafficLight is one of the following Strings:
; "green"
; "red"
; "yellow"
; interpretation is the state of the traffic light

(define BACKG
  (empty-scene WIDTH HEIGHT))

; TrafficLight -> TrafficLight
; yields the next state, given current state cs

;; tests
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "red") "green")

(define (tl-next cs)
  (cond
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]
    [(string=? cs "red") "green"]))

; TrafficLight -> Image
; places the traffic light image to BACKG

;; tests
(check-expect
 (tl-render "green")
 (place-image
  (traffic-light "green")
  (/ WIDTH 2)
  (/ HEIGHT 2)
  BACKG))

(check-expect
 (tl-render "yellow")
 (place-image
  (traffic-light "yellow")
  (/ WIDTH 2)
  (/ HEIGHT 2)
  BACKG))

(check-expect
 (tl-render "red")
 (place-image
  (traffic-light "red")
  (/ WIDTH 2)
  (/ HEIGHT 2)
  BACKG))

(define (tl-render cs)
  (place-image
   (traffic-light cs)
   (/ WIDTH 2)
   (/ HEIGHT 2)
   BACKG))

; TrafficLight -> Image
; renders the traffic light based on the current
; state.

(check-expect
 (traffic-light "green")
 (beside
   (circle LIGHT-SIZE "outline" "red")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "outline" "yellow")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "solid" "green")))

(check-expect
 (traffic-light "yellow")
 (beside
   (circle LIGHT-SIZE "outline" "red")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "solid" "yellow")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "outline" "green")))

(check-expect
 (traffic-light "red")
 (beside
   (circle LIGHT-SIZE "solid" "red")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "outline" "yellow")
   (rectangle 5 LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE "outline" "green")))

(define (traffic-light cs)
   (beside
   (circle LIGHT-SIZE (light-mode cs "red") "red")
   (rectangle GAP LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE (light-mode cs "yellow") "yellow")
   (rectangle GAP LIGHT-SIZE "solid" "white")
   (circle LIGHT-SIZE (light-mode cs "green") "green")))

; TrafficLight TrafficLight -> mode
; returns the mode based on the current state
; and the traffic light.
; mode can be one of the following:
; 'solid, "solid", 'outline, or "outline"

(check-expect (light-mode "green" "green")
              "solid")
(check-expect (light-mode "green" "green")
              "solid")
(check-expect (light-mode "red" "red")
              "solid")
(check-expect (light-mode "green" "red")
              "outline")
(check-expect (light-mode "yellow" "green")
              "outline")

(define (light-mode cs tl)
  (if (string=? cs tl) "solid" "outline"))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))
