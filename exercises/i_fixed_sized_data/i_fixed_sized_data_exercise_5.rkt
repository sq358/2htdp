; Excercise 5.
(require 2htdp/image)

(define WIDTH 200)
(define HEIGHT 100)

(define DECK-COLOR "aquamarine")

(define (draw-boat w h)
(overlay/xy
 (overlay/xy (draw-left-deck w h)
             (/ w 2)
              0
              (draw-center-deck w h))
 (+ (/ w 2) w)
 0
(draw-right-deck w h)))

(define (draw-left-deck w h)
   (triangle/sas h 90 (/ w 2) "solid" DECK-COLOR))

(define (draw-center-deck w h)
   (rectangle w h "solid" "aquamarine"))

(define (draw-right-deck w h)
   (triangle/ass 90 h (/ w 2) "solid" DECK-COLOR))
