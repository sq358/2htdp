; Excersice 47.
(require 2htdp/universe)

; WorldState is a Number
; interpretation is the level of happiness.

(define MIN-SCORE 0)
(define MAX-SCORE 100)

(define WIDTH-OF-WORLD MAX-SCORE)
(define HEIGHT-OF-WORLD (/ MAX-SCORE 4))

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define GUAGE
  (rectangle MAX-SCORE HEIGHT-OF-WORLD "solid" "red"))

(define DEPLETION-RATE .1)

; WorldState -> WorldState
; for each clock tick If the new ws is above
; MAX-SCORE returns MAX-SCORE.
; else, computes deplete.
; If the new ws is below MIN-SCORE returns 0
; othrewise returns ws.

(check-expect (toc 150) MAX-SCORE)
(check-expect (toc 100) (- 100 DEPLETION-RATE))
(check-expect (toc 20) (- 20 DEPLETION-RATE))
(check-expect (toc 0) 0)
(check-expect (toc -1) 0)

(define (toc ws)
  (cond
    [(> ws MAX-SCORE) MAX-SCORE]
    [(<= (deplete ws) MIN-SCORE) 0]
    [else (deplete ws)]))

(define (deplete ws)
  (- ws DEPLETION-RATE))

; WorldState String -> WorldState
; if ke is 'j' increases the ws by 1/5, otherwise
; if ke is 'k' increase the ws by 1/3.

(check-expect (happify 10 "j")
              (+ 10 (* 10 1/5)))
(check-expect (happify 20 "k")
              (+ 20 (* 20 1/3)))
(check-expect (happify 10 "u") 10)

(define (happify ws ke)
  (cond
    [(string=? ke "j") (+ ws (* ws 1/5))]
    [(string=? ke "k") (+ ws (* ws 1/3))]
    [else ws]))

; WorldState -> Image
; if world state is equal or greater than 0
; places happiness-guage on BACKGROUND
; else renders only BACKGOUND

(check-expect (render 0) BACKGROUND)
(check-expect (render 50)
              (place-image
               (rectangle 50 HEIGHT-OF-WORLD "solid" "red")
                (/ 50 2) HEIGHT-OF-WORLD BACKGROUND))
(check-expect (render 100)
              (place-image
               (rectangle 100 HEIGHT-OF-WORLD "solid" "red")
                (/ 100 2) HEIGHT-OF-WORLD BACKGROUND))

(define (render ws)
  (cond
    [(<= ws 0) BACKGROUND]
    [else
     (place-image (happiness-guage ws)
      (/ ws 2) HEIGHT-OF-WORLD BACKGROUND)]))

(check-expect (happiness-guage -1)
              (rectangle 0 HEIGHT-OF-WORLD "solid" "red"))
(check-expect (happiness-guage 0)
              (rectangle 0 HEIGHT-OF-WORLD "solid" "red"))
(check-expect (happiness-guage 10)
              (rectangle 10 HEIGHT-OF-WORLD "solid" "red"))
(check-expect (happiness-guage 100)
              (rectangle 100 HEIGHT-OF-WORLD "solid" "red"))

(define (happiness-guage ws)
  (cond
    [(<= ws 0) (rectangle 0 HEIGHT-OF-WORLD "solid" "red")]
    [else (rectangle ws HEIGHT-OF-WORLD "solid" "red")]))

(define (guage-prog ws)
  (big-bang ws
    [on-tick toc]
    [on-key happify]
    [to-draw render]))
