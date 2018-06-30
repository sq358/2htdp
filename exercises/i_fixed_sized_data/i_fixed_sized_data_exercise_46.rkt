; Exercise 46.
(require 2htdp/universe)

; WorldState is a Number
; interpretation is the number of pixels from left

; ------------------------------------
; Ucomment the following lines and
; replace <image*> with an actual images
; ------------------------------------
; (define CAT1 <image1>)
; (define CAT2 <image2>)

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD
  (* (image-height CAT1) 2))

(define CAT-BOUNDARY
  (+ WIDTH-OF-WORLD (/ (image-width CAT1) 2)))

(define Y-CAT
  (image-height CAT1))

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; WorldState -> WorldState
; for every clock tick if world state plus 3 is
; within CAT-BOUNDARY returns the addition
; otherwise returns 0
;
; Considering the CAT context, for every clock
; tick it moves the CAT within the CAT-BOUNDARY

(check-expect (tock 50) 53)
(check-expect (tock 77) 80)
(check-expect (tock 120) 123)
(check-expect (tock (+ CAT-BOUNDARY 1)) 3)

(define (tock ws)
  (cond
    [(>= (+ ws 3) CAT-BOUNDARY 2) 3]
    [else (+ ws 3)]))

; WorldState -> Image
; places CAT1 or CAT2 to x pixels from the left
; of the BACKGROUD where x is the current
; world state.
;
; Uses CAT1 if x is even and CAT2 if x is odd

(check-expect (render 50)
              (place-image CAT1 50 Y-CAT BACKGROUND))
(check-expect (render 101)
              (place-image CAT2 101 Y-CAT BACKGROUND))
(check-expect (render 150)
              (place-image CAT1 150 Y-CAT BACKGROUND))

(define (render ws)
  (cond
    [(= (modulo ws 2) 0)
     (place-image CAT1 ws Y-CAT BACKGROUND)]
    [else
     (place-image CAT2 ws Y-CAT BACKGROUND)]))

(define (cat-prog ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))
