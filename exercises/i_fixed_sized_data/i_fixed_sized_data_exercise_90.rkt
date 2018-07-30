; Exercise 90.

(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)

(define MTSN
  (empty-scene WIDTH HEIGHT))

(define-struct vcat [location happiness])
; VCat is a structure:
;  (define-struct vcat [Number Number])
; interpretation (make-vcat x h) is a cat
;  at x-coordinate with the happiness score
;  h


(define CAT1 .)
(define CAT2 .)

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD
  (* (image-height CAT1) 2))

(define CAT-BOUNDARY
  (+ WIDTH-OF-WORLD (/ (image-width CAT1) 2)))

(define Y-CAT
  (image-height CAT1))

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define DEPLETION-RATE .1)

(define MIN-SCORE 0)
(define MAX-SCORE 100)

(define WIDTH-OF-GUAGE 10)
(define HEIGHT-OF-GUAGE 10)

(define GUAGE
  (rectangle WIDTH-OF-GUAGE HEIGHT-OF-GUAGE "solid" "red"))

(define (happy-cat ws)
  (big-bang (make-vcat ws 100)
    [on-tick toc]
    [to-draw render]
    [on-key happify]
    [stop-when stop?]))

; VCat -> VCat
; For every clock tick:
; - if cat-location plus 3 is within CAT-BOUNDARY
;   returns the addition otherwise returns 0 for
;   cat-location
;
; -  If the new cat-happiness is above MAX-SCORE
;    returns cat-happiness equal to MAX-SCORE.
;    If not computes deplete, if the new cat-happiness
;    is below MIN-SCORE returns cat-happiness equal 0
;    othrewise returns returns cat-happiness.

(check-expect (toc (make-vcat 50 150))
              (make-vcat 53 MAX-SCORE))

(check-expect (toc (make-vcat 77 100))
              (make-vcat 80  (- 100 DEPLETION-RATE)))

(check-expect (toc (make-vcat 120 20))
              (make-vcat 123 (- 20 DEPLETION-RATE)))

(check-expect (toc (make-vcat (+ CAT-BOUNDARY 1) 0))
              (make-vcat 3 0))

(check-expect (toc (make-vcat (+ CAT-BOUNDARY 1) -1))
              (make-vcat 3 0))

(define (toc cat)
  (make-vcat
   (cond
    [(>= (+ (vcat-location cat) 3) CAT-BOUNDARY 2) 3]
    [else (+ (vcat-location cat) 3)])
   (cond
    [(> (vcat-happiness cat) MAX-SCORE) MAX-SCORE]
    [(<= (deplete (vcat-happiness cat)) MIN-SCORE) 0]
    [else (deplete (vcat-happiness cat))]
   )))

(define (deplete ws)
  (- ws DEPLETION-RATE))

; VCat -> Image
; - places CAT1 or CAT2 to x pixels from the left
;   of the BACKGROUD where x is the current
;   vcat-location
;
;   Uses CAT1 if vcat-location is even and CAT2 if
;   if vcat-location is odd
;
; - if cat-happiness is equal or greater than 0
;   places happiness-guage on MTSN does nothing

(check-expect (render (make-vcat 50 0))
              (place-image
               CAT1
               50
               Y-CAT
               (place-image
                (happiness-guage 0)
                0
                HEIGHT-OF-GUAGE
                BACKGROUND)))

(check-expect (render (make-vcat 101 50))
              (place-image
               CAT2
               101
               Y-CAT
               (place-image
                (happiness-guage 50)
                (/ (vcat-happiness (make-vcat 101 50)) 2)
                HEIGHT-OF-GUAGE
                BACKGROUND)))

(define (render cat)
  (place-image
   (cond
     [(= (modulo (vcat-location cat) 2) 0) CAT1]
     [else CAT2])
   (vcat-location cat)
   Y-CAT
   (place-image
    (happiness-guage (vcat-happiness cat))
    (/ (vcat-happiness cat) 2)
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


; VCat String -> VCat
; if ke is 'j' increases the ws by 1/5, otherwise
; if ke is 'k' increase the ws by 1/3.

(check-expect (happify (make-vcat 100 10) "j")
              (make-vcat 100 (+ 10 (* 10 1/5))))

(check-expect (happify (make-vcat 100 20) "k")
              (make-vcat 100 (+ 20 (* 20 1/3))))

(check-expect (happify (make-vcat 100 10) "u")
              (make-vcat 100 10))

(define (happify cat ke)
  (cond
    [(string=? ke "j")
     (make-vcat
      (vcat-location cat)
      (+ (vcat-happiness cat) (* (vcat-happiness cat) 1/5)))]
    [(string=? ke "k")
     (make-vcat
      (vcat-location cat)
      (+ (vcat-happiness cat) (* (vcat-happiness cat) 1/3)))]
    [else cat]))

(define (stop? cat)
  (zero? (vcat-happiness cat)))
