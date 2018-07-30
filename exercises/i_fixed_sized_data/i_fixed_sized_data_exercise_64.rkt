; Exercise 64.

; Posn -> Number
; computes the manhattan distance to the origin.

(check-expect
 (manhattan-distance (make-posn 0 0)) 0)
(check-expect
 (manhattan-distance (make-posn 0 1)) 1)
(check-expect
 (manhattan-distance (make-posn 3 4)) 7)
(check-expect
 (manhattan-distance (make-posn 2 2)) 4)

(define (manhattan-distance p)
  (+ (posn-x p) (posn-y p)))
