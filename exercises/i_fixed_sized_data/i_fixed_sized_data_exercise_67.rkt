; Exercise 67.

(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")

; with balld representing location and direction
; and speed as constant, we're tying a property
; of bouncing ball (speed) to the world state.
; if the problem domain is the physical world,
; perhaps this is not a good idea.
