; Exercise 60.
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

; An N-TrafficLight is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow

(define BACKG
  (empty-scene WIDTH HEIGHT))

; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs

;; tests
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)

(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

;; question: Does the tl-next function convey its
;;  intention more clearly than the tl-next-numeric
;;  function? If so, why? If not, why not?
;;
;; answer: It's not, mainly because using strings
;;  as TrafficLight is more expressive.

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
    [on-tick tl-next-numeric 1]))
