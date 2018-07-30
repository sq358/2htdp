; Exercise 74.
(require 2htdp/universe)

; A Posn represents the state of the world.
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p

;; tests
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))

; Posn -> Image
; places DOT with Posn p coordinates on the MTS.

;; tests
(check-expect
 (render (make-posn 3 4)) (place-image DOT 3 4 MTS))
(check-expect
 (render (make-posn 20 40)) (place-image DOT 20 40 MTS))

(define (render p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

(define (main p)
  (big-bang p
    [on-mouse reset-dot]
    [to-draw render]))
