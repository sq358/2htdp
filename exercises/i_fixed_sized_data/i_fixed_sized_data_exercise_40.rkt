; Exercise 40.
; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick

(check-expect (tock 20) 22)
(check-expect (tock 78) 81)

(define (tock ws)
  (+ 3 ws))
