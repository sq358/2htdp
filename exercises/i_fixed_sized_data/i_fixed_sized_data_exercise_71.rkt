; Exercise 71.

(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH 400)
(define CENTER (quotient WIDTH 2))

(define-struct game [left-player right-player ball])

(define game0
  (make-game MIDDLE MIDDLE (make-posn  CENTER CENTER)))

;; (game-ball game0)
;;  1. (game-ball (make-game 100 100 (make-posn 200 200)))
;;  2. (make-posn 200 200)

;; (posn? (game-ball game0))
;;  1. (posn? (make-posn 200 200))
;;  2. #true
;;
;; Correction from stepper:
;;  1. (posn? (game-ball
;;              (make-game 100 100 (make-posn 200 200))
;;  2. (osn? (make-posn 200 200))
;;  3. #true

;; (game-left-player game0)
;;  1. (game-left-player
;;      (make-game 100 100 (make-posn 200 200)))
;;  2. 100
