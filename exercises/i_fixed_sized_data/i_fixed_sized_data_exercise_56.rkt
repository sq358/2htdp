; Exercise 56.

(require 2htdp/universe)

(define HEIGHT 300) ; distances in pixels
(define WIDTH  100)
(define YDELTA 30)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; LRCD -> Image
; renders the state as a resting or flying rocket

;; test
(check-expect (show "resting")
              (place-image
               ROCKET 10 (- HEIGHT CENTER) BACKG))

(check-expect (show -2)
              (place-image
               ROCKET 10 (- HEIGHT CENTER)
               (place-image
                 (text "-2" 20 "red")
                 10 (* 3/4 WIDTH) BACKG)))

(check-expect (show 40)
              (place-image
               ROCKET 10 (- 40 CENTER) BACKG))

(define (show x)
  (place-image
   ROCKET 10 (- (height x) CENTER)
   (cond
     [(count-down? x)
      (place-image
       (text (number->string x) 20 "red")
       10 (* 3/4 WIDTH) BACKG)]
     [else BACKG])))

(define (height x)
  (cond
    [(resting? x) HEIGHT]
    [else x]))

(define (resting? x)
  (or (string? x) (count-down? x)))

(define (count-down? x)
  (cond
    [(string? x) #false]
    [(<= -3 x -1) #true]
    [else #false]))


; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting

;; tests
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

; LRCD -> Boolean
; determines if big-bang execution should stop

(check-expect (stop? 10) #false)
(check-expect (stop? "resting") #false)
(check-expect (stop? " ") #false)
(check-expect (stop? -1) #false)
(check-expect (stop? (* -1 (image-height ROCKET))) #true)

(define (stop? x)
  (cond
    [(string? x) #false]
    [(<= -3 x -1) #false]
    [(>= x 0) #false]
    [(< x -3) #true]))

; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-tick fly 1]
    [on-key launch]
    [stop-when stop?]))
