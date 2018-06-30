; Exercise 51.

; What is the most appropriate initial state?
; Red because it's ensures the safer to start a light
; that instructs the drivers to stop.

(require 2htdp/universe)

; WorldState is one of these Strings:
; - "Red"
; - "Green"
; - "Yellow"
; interpretation Image is a circle with
; representing the traffic light.

(define BACKGROUND
  (empty-scene 100 100))

(define TRAFFIC-LIGHT-SIZE 50)

; WorldState -> WorldState
; for each clock tick returns the next
; WorldState.

(check-expect (toc "red") "green")
(check-expect (toc "green") "yellow")
(check-expect (toc "yellow") "red")

(define (toc ws)
  (cond
    [(string=? ws "red") "green"]
    [(string=? ws "green") "yellow"]
    [(string=? ws "yellow") "red"]))

; WorldState -> Image
; places the traffic-color to the BACKGROUND.

(check-expect (render "red")
              (place-image (traffic-light "red") 50 50 BACKGROUND))
(check-expect (render "green")
              (place-image (traffic-light "green") 50 50 BACKGROUND))
(check-expect (render "yellow")
              (place-image (traffic-light "yellow") 50 50 BACKGROUND))

(define (render ws)
  (place-image (traffic-light ws) 50 50 BACKGROUND))

(check-expect (traffic-light "red")
               (circle TRAFFIC-LIGHT-SIZE "solid" "red"))

(define (traffic-light color)
  (circle TRAFFIC-LIGHT-SIZE "solid" color))

(define (main ws)
  (big-bang ws
    [on-tick toc]
    [to-draw render]))
