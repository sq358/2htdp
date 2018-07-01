; Exercise 55.
(check-expect (show-aux "resting")
              (place-image
               ROCKET 10 (- HEIGHT CENTER) BACKG))

(check-expect (show-aux -2)
              (place-image
               ROCKET 10 (- HEIGHT CENTER)
               (place-image
                 (text "-2" 20 "red")
                 10 (* 3/4 WIDTH) BACKG)))

(check-expect (show-aux 40)
              (place-image
               ROCKET 10 (- 40 CENTER) BACKG))

(define (show-aux x)
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
  (or (and (string? x) (string=? x "resting"))
      (count-down? x)))

(define (count-down? x)
  (cond
    [(string? x) #false]
    [(<= -3 x -1) #true]
    [else #false]))
