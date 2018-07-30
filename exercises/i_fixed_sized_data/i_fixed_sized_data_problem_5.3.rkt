; Sample Problem 5.3.

(define-struct r3 [x y z])
; An R3 is a structure:
;   (make-r3 Number Number Number)

(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))


; R3 -> Number
; computes the distance of p to the origin

(check-within
 (r3-distance-to-0 ex1) (sqrt 174) .001)
(check-within
 (r3-distance-to-0 ex2) (sqrt 10) .001)

(define (r3-distance-to-0 p)
  (sqrt
   (+ (sqr (r3-x p)) (sqr (r3-y p)) (sqr (r3-z p)))))
