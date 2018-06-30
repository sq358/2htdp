; Exercise 49.

; y is 100
(- 200 (cond [(> 100 200) 0] [else 100]))

; y is 210
(- 200 (cond [(> 210 200) 0] [else 210]))

(define (creaet-rocket-scene.v5 h)
  (place-image
   ROCKET
   50
   (cond
    [(<= h ROCKET-CENTER-TOP) h]
    [(> h ROCKET-CENTER-TO-TOP) ROCKET-CENTER-TO-TOP])
   MTSCN))
