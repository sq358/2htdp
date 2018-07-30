; Exercise 91.

(require 2htdp/universe)

(define-struct vcham [location color happiness])
; VCham is a structure:
;  (define-struct vcham [Number String Number])
; interpretation (make-vcham x "blue" h) is a blue
; chameleon at x-coordinate with the happiness score


(define CHAM .)

(define (cham color)
  (overlay CHAM
           (rectangle (image-width CHAM)
             (image-height CHAM)
             "solid"
             color)))

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD
  (* (image-height CHAM) 2))

(define BOUNDARY
  (+ WIDTH-OF-WORLD (/ (image-width CHAM) 2)))

(define Y-CHAM
  (image-height CHAM))

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define DEPLETION-RATE .1)

(define MIN-SCORE 0)
(define MAX-SCORE 100)

(define WIDTH-OF-GUAGE 10)
(define HEIGHT-OF-GUAGE 10)

(define GUAGE
  (rectangle WIDTH-OF-GUAGE HEIGHT-OF-GUAGE "solid" "red"))

(define (happy-cham x)
  (big-bang (make-vcham x "white" 100)
    [on-tick toc]
    [to-draw render]
    [on-key modify-cham]
    [stop-when stop?]))

; VCham -> VCham
; For every clock tick:
; - if cham-location plus 3 is within BOUNDARY
;   returns the addition otherwise returns 0 for
;   cham-location
;
; - If the new cham-happiness is above MAX-SCORE
;   returns cham-happiness equal to MAX-SCORE.
;   If not, computes deplete, if the new cham-happiness
;   is below MIN-SCORE returns cham-happiness equal 0
;   othrewise returns returns cham-happiness.

(check-expect (toc (make-vcham 50 "white" 150))
              (make-vcham 53 "white" MAX-SCORE))

(check-expect (toc (make-vcham 77 "white" 100))
              (make-vcham 80 "white" (- 100 DEPLETION-RATE)))

(check-expect (toc (make-vcham 120 "white" 20))
              (make-vcham 123 "white" (- 20 DEPLETION-RATE)))

(check-expect (toc (make-vcham (+ BOUNDARY 1) "white" 0))
              (make-vcham 3 "white" 0))

(check-expect (toc (make-vcham (+ BOUNDARY 1) "white" -1))
              (make-vcham 3 "white" 0))

(define (toc cham)
  (make-vcham
   (cond
    [(>= (+ (vcham-location cham) 3) BOUNDARY 2) 3]
    [else (+ (vcham-location cham) 3)])
   (vcham-color cham)
   (cond
    [(> (vcham-happiness cham) MAX-SCORE) MAX-SCORE]
    [(<= (deplete (vcham-happiness cham)) MIN-SCORE) 0]
    [else (deplete (vcham-happiness cham))]
   )))

(define (deplete ws)
  (- ws DEPLETION-RATE))

; VCham -> Image
; - places CHAM to x pixels from the left
;   of the BACKGROUD where x is the current
;   vcham-location
;
; - if cham-happiness is equal or greater than 0
;   places happiness-guage on MTSN does nothing

(check-expect (render (make-vcham 50 "white" 0))
              (place-image
               (cham "white")
               50
               Y-CHAM
               (place-image
                (happiness-guage 0)
                0
                HEIGHT-OF-GUAGE
                BACKGROUND)))

(check-expect (render (make-vcham 101 "red" 50))
              (place-image
               (cham "red")
               101
               Y-CHAM
               (place-image
                (happiness-guage 50)
                (/ (vcham-happiness (make-vcham 101 "red" 50)) 2)
                HEIGHT-OF-GUAGE
                BACKGROUND)))

(define (render ch)
  (place-image
   (cham (vcham-color ch))
   (vcham-location ch)
   Y-CHAM
   (place-image
    (happiness-guage (vcham-happiness ch))
    (/ (vcham-happiness ch) 2)
    HEIGHT-OF-GUAGE
    BACKGROUND)))

(check-expect (happiness-guage -1)
              (rectangle 0 HEIGHT-OF-GUAGE "solid" "red"))
(check-expect (happiness-guage 0)
              (rectangle 0 HEIGHT-OF-GUAGE "solid" "red"))
(check-expect (happiness-guage 10)
              (rectangle 10 HEIGHT-OF-GUAGE "solid" "red"))
(check-expect (happiness-guage 100)
              (rectangle 100 HEIGHT-OF-GUAGE "solid" "red"))

(define (happiness-guage ws)
  (cond
    [(<= ws 0) (rectangle 0 HEIGHT-OF-GUAGE "solid" "red")]
    [else (rectangle ws HEIGHT-OF-GUAGE "solid" "red")]))


; VCham String -> VCham
; if ke is 'j' increases the vcham-happiness by 2

(check-expect (modify-cham (make-vcham 100 "white" 10) "j")
              (make-vcham 100 "white" 12))

(check-expect (modify-cham (make-vcham 100 "white" 10) "b")
              (make-vcham 100 "blue" 10))

(check-expect (modify-cham (make-vcham 100 "white" 10) "r")
              (make-vcham 100 "red" 10))

(check-expect (modify-cham (make-vcham 100 "white" 10) "g")
              (make-vcham 100 "green" 10))


(check-expect (modify-cham (make-vcham 100 "white" 10) "u")
              (make-vcham 100 "white" 10))

(define (modify-cham ch ke)
  (cond
    [(string=? ke "j")
     (make-vcham
      (vcham-location ch)
      (vcham-color ch)
      (+ (vcham-happiness ch) 2))]
    [(string=? ke "w")
     (make-vcham
      (vcham-location ch)
      "white"
      (vcham-happiness ch))]
    [(string=? ke "b")
     (make-vcham
      (vcham-location ch)
      "blue"
      (vcham-happiness ch))]
    [(string=? ke "g")
     (make-vcham
      (vcham-location ch)
      "green"
      (vcham-happiness ch))]
    [(string=? ke "r")
     (make-vcham
      (vcham-location ch)
      "red"
      (vcham-happiness ch))]
    [else
     (make-vcham
      (vcham-location ch)
      (vcham-color ch)
      (vcham-happiness ch))]))

(define (stop? ch)
  (<= (vcham-happiness ch) 0))
