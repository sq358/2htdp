; Exercise 73.

; Posn Number -> Posn
; for posn p and number n, produces a posn
; with n as its x field.

(check-expect
 (posn-up-x (make-posn 2 3) 4) (make-posn 4 3))

(check-expect
 (posn-up-x (make-posn -1 6) 1) (make-posn 1 6))

(check-expect
 (posn-up-x (make-posn 0 0) 5) (make-posn 5 0))

(define (posn-up-x p n)
  (make-posn n (posn-y p)))
