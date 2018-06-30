; Excerise 2.

; constants
(define WIDTH 200)
(define HEIGHT 400)
(define V 3)
(define X 50)
(define MTSCN (empty-scene WIDTH HEIGHT "blue"))
(define ROCKET .)

(define ROCK-HEIGHT 10)
(define ROCK (rectangle 20 ROCK-HEIGHT "solid" "brown"))

(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (+ ROCK-HEIGHT (/ (image-height ROCKET) 2))))

; functions
(define (picture-of-rocket.v6 t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X (distance t) MTSCN)]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X ROCKET-CENTER-TO-TOP MTSCN)]))
