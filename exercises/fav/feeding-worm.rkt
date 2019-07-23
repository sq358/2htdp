;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_215) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

(require 2htdp/universe)

(define-struct worm-segment [from-left from-top])
; a WormSegment is a Posn
; interpretation:
;  (make-posn 6 10) is a distance 6 worm segments
; from left and 10 worm segments from the top

; A Tail is one of:
; - '()
; - (cons WormSegment WormTail)

(define-struct worm [tail direction])
; A Worm is a structure:
;  (make-worm (list (make-posn Number Number)) String)
; Interpretation:
;  (make-worm (list (make-posn 5 4)) "left") is a 1 segment worm
; that is located in the position 5 worm segments widths from
; the left and 4 worm segments width from top

(define-struct food [location])
; A Food is a structure:
;  (make-food Posn)
; interpretation:
; (make-food (make-posn 10 30)) is a food location
; at (make-posn 10 30)

(define-struct world-state [worm food])
; A WorldState is a structure:
; (make-world-state Worm Feed)
; interpretation:
; (make-world-state
;  (make-worm (list (make-posn 10 30)) "left")
;  (make-food (make-posn 4 5)))
; is a worm moving towards left and a food locatad at
; location (make-posn 4 5)

(define SCENE-WIDTH 200)
(define SCENE-HEIGHT 200)

(define MTSCN
  (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define WORM-COLOR "blue")
(define WORM-SEGMENT-SIZE 3)
(define WORM-SEGMENT
  (circle WORM-SEGMENT-SIZE "solid" WORM-COLOR))
(define FOOD-COLOR "green")
(define FOOD-SIZE WORM-SEGMENT-SIZE)
(define FOOD
  (circle FOOD-SIZE "solid" FOOD-COLOR))

(define GAME-OVER-MSG-WALL
  (text "Game Over: Worm hit the wall!" 10 "red"))

(define GAME-OVER-MSG-WORM
  (text "Game Over: Worm ran into itsef!" 10 "red"))

;; test constants

(define TEST-WORM-LEFT
  (make-worm
   (list
    (make-posn 10 6) (make-posn 11 6)) "left"))

(define TEST-WORM-RIGHT
  (make-worm
   (list
    (make-posn 10 6) (make-posn 11 6)) "right"))

(define TEST-WORM-UP
  (make-worm
   (list
    (make-posn 10 6) (make-posn 11 6)) "up"))

(define TEST-WORM-DOWN
  (make-worm
   (list
    (make-posn 10 6) (make-posn 11 6)) "down"))

(define TEST-WORM
  (make-worm
   (list
    (make-posn 10 6)
    (make-posn 11 6)
    (make-posn 12 6)
    (make-posn 13 6)
    (make-posn 14 6)
    (make-posn 15 6)
    (make-posn 16 6)) "down"))

(define TEST-FOOD-1 (make-food (make-posn 10 7)))
(define TEST-FOOD-2 (make-food (make-posn 17 7)))

(define TEST-WS-TO-EAT
  (make-world-state TEST-WORM TEST-FOOD-1))

(define TEST-WS-NOT-EATEN
  (make-world-state TEST-WORM TEST-FOOD-2))

; main
(define (worm-main clockrate)
  (big-bang TEST-WS-NOT-EATEN
    [to-draw render-ws]
    [on-tick toc clockrate]
    [on-key control-ws]
    [stop-when game-over? show-game-over-msg]))

; WorldState -> Image
; renders the world state on empty scene

(check-expect
 (render-ws TEST-WS-TO-EAT)
 (place-image
  FOOD
  (* (posn-x (food-location TEST-FOOD-1))
     (image-width FOOD))
  (* (posn-y (food-location TEST-FOOD-1))
     (image-width FOOD))
  (render-worm-tail (worm-tail TEST-WORM))))

(define (render-ws ws)
  (place-image
   FOOD
   (* (posn-x (food-location (world-state-food ws)))
      (image-width FOOD))
   (* (posn-y (food-location (world-state-food ws)))
      (image-width FOOD))
   (render-worm-tail (worm-tail (world-state-worm ws)))))

; Worm -> Image
; renders the worm on empty scene

(define (render-worm w)
  (render-worm-tail (worm-tail w)))

; Tail -> Image
; renders the worm tail on empty scene

(check-expect
 (render-worm-tail (worm-tail TEST-WORM-LEFT))
 (place-image
  WORM-SEGMENT
  (*
   (posn-x
    (first (worm-tail TEST-WORM-LEFT)))
   (image-width WORM-SEGMENT))
  (*
   (posn-y
    (first (worm-tail TEST-WORM-LEFT)))
   (image-width WORM-SEGMENT))
  (place-image
   WORM-SEGMENT
   (*
    (posn-x
     (second (worm-tail TEST-WORM-LEFT)))
    (image-width WORM-SEGMENT))
   (*
    (posn-y
     (second (worm-tail TEST-WORM-LEFT)))
    (image-width WORM-SEGMENT))
  MTSCN)))

(define (render-worm-tail tail)
  (cond
    [(empty? tail) MTSCN]
    [else
     (place-image
      WORM-SEGMENT
      (*
       (posn-x (first tail))
       (image-width WORM-SEGMENT))
      (*
       (posn-y (first tail))
       (image-width WORM-SEGMENT))
      (render-worm-tail (rest tail)))]))

; Worm -> Worm
; moves the worm with one worm's diameter based on worm-segment-distance

(check-expect
 (move-worm TEST-WORM-LEFT)
 (make-worm
  (move-tail (worm-tail TEST-WORM-LEFT) (worm-direction TEST-WORM-LEFT))
  (worm-direction TEST-WORM-LEFT)))

(define (move-worm w)
  (make-worm
   (move-tail (worm-tail w) (worm-direction w))
   (worm-direction w)))

; WorldState -> WorldState
; for every clock tick produces the next world state
; based on the current world state

(check-expect
 (toc TEST-WS-NOT-EATEN)
 (make-world-state
  (make-worm
   (move-tail
    (worm-tail (world-state-worm TEST-WS-NOT-EATEN))
    (worm-direction (world-state-worm TEST-WS-NOT-EATEN)))
   (worm-direction (world-state-worm TEST-WS-NOT-EATEN)))
  (world-state-food TEST-WS-NOT-EATEN)))

(check-random
 (toc TEST-WS-TO-EAT)
 (make-world-state
  (make-worm
   (cons
    (make-posn
     (posn-x (food-location (world-state-food TEST-WS-TO-EAT)))
     (posn-y (food-location (world-state-food TEST-WS-TO-EAT))))
    (worm-tail (world-state-worm TEST-WS-TO-EAT)))
   (worm-direction (world-state-worm TEST-WS-TO-EAT)))
  (make-food
   (food-create
    (food-location
     (world-state-food TEST-WS-TO-EAT))))))

(define (toc ws)
  (if (about-to-eat? ws)
      (make-world-state
       (make-worm
        (cons
         (make-posn
          (posn-x (food-location (world-state-food ws)))
          (posn-y (food-location (world-state-food ws))))
         (worm-tail (world-state-worm ws)))
        (worm-direction (world-state-worm ws)))
       (make-food
        (food-create
         (food-location
          (world-state-food ws)))))
      (make-world-state
       (make-worm
        (move-tail
         (worm-tail (world-state-worm ws))
         (worm-direction (world-state-worm ws)))
        (worm-direction (world-state-worm ws)))
       (world-state-food ws))))

; WorldState -> Boolean
; determines if a worm is about to eat the food

(define (about-to-eat? ws)
  (cond
    [(string=? (worm-direction (world-state-worm ws)) "left")
     (and (= (- (posn-x
                 (first (worm-tail (world-state-worm ws))))
                (posn-x (food-location (world-state-food ws)))) 1)
          (= (posn-y
              (first (worm-tail (world-state-worm ws))))
             (posn-y (food-location (world-state-food ws)))))]
    [(string=? (worm-direction (world-state-worm ws)) "right")
     (and (= (- (posn-x
                 (first (worm-tail (world-state-worm ws))))
                (posn-x (food-location (world-state-food ws)))) -1)
          (= (posn-y
              (first (worm-tail (world-state-worm ws))))
             (posn-y (food-location (world-state-food ws)))))]
    [(string=? (worm-direction (world-state-worm ws)) "up")
     (and (= (posn-x
              (first (worm-tail (world-state-worm ws))))
             (posn-x (food-location (world-state-food ws))))
          (= (- (posn-y
                 (first (worm-tail (world-state-worm ws))))
                (posn-y (food-location (world-state-food ws)))) 1))]
    [(string=? (worm-direction (world-state-worm ws)) "down")
     (and (= (posn-x
              (first (worm-tail (world-state-worm ws))))
             (posn-x (food-location (world-state-food ws))))
          (= (- (posn-y
                 (first (worm-tail (world-state-worm ws))))
                (posn-y (food-location (world-state-food ws)))) -1))]))

; Tail String -> Tail
; moves tail in the direction s

(check-expect (move-tail '() "left") '())
(check-expect
 (move-tail
  (list (make-posn 10 40) (make-posn 11 40)) "left")
 (list (make-posn 9 40) (make-posn 10 40)))
(check-expect
 (move-tail
  (list (make-posn 11 40) (make-posn 10 40)) "right")
 (list (make-posn 12 40) (make-posn 11 40)))
(check-expect
 (move-tail
  (list (make-posn 10 40) (make-posn 11 40)) "up")
 (list (make-posn 10 39) (make-posn 10 40)))
(check-expect
 (move-tail
  (list (make-posn 10 40) (make-posn 11 40)) "down")
 (list (make-posn 10 41) (make-posn 10 40)))

(define (move-tail t d)
  (cond
    [(empty? t) '()]
    [else
     (cond
       [(string=? d "left")
        (cons
         (make-posn
          (- (posn-x (first t)) 1)
          (posn-y (first t))) (remove-last t))]
       [(string=? d "right")
        (cons
         (make-posn
          (+ (posn-x (first t)) 1)
          (posn-y (first t))) (remove-last t))]
       [(string=? d "up")
        (cons
         (make-posn
          (posn-x (first t))
          (- (posn-y (first t)) 1)) (remove-last t))]
       [(string=? d "down")
        (cons
         (make-posn
          (posn-x (first t))
          (+ (posn-y (first t)) 1)) (remove-last t))])]))

; Tail -> Tail
; removes the last segment of the tails

(check-expect (remove-last '()) '())
(check-expect
 (remove-last
   (list (make-posn 10 40) (make-posn 11 40)))
  (list (make-posn 10 40)))

(define (remove-last t)
  (cond
    [(empty? t) '()]
    [else
     (if (empty? (rest t)) '()
         (cons (first t) (remove-last (rest t))))]))

; Worm -> Boolean
; determines if the game is over

(check-expect (game-over? TEST-WS-NOT-EATEN) #false)
(check-expect
 (game-over?
  (make-world-state
   (make-worm
    (list
     (make-posn 11 6)
     (make-posn 11 7)
     (make-posn 10 7)
     (make-posn 10 6)
     (make-posn 11 6)) "up")
   TEST-FOOD-2)) #true)

(define (game-over? ws)
  (cond
    [(empty? (worm-tail (world-state-worm ws))) '()]
    [else
     (or
      (member
       (first (worm-tail (world-state-worm ws)))
       (rest (worm-tail (world-state-worm ws))))
      (worm-hit-the-wall? (world-state-worm ws)))]))

; WorldState -> Image
; renders the game over state of the game

(check-expect
 (show-game-over-msg
  (make-world-state
   (make-worm
    (list
     (make-posn -1 6)
     (make-posn 1 6)
     (make-posn 2 6)
     (make-posn 3 6)
     (make-posn 4 6)) "left")
   TEST-FOOD-2))
 (place-image
  GAME-OVER-MSG-WALL
  (- SCENE-WIDTH (/ (image-width GAME-OVER-MSG-WORM) 2))
  (- SCENE-HEIGHT (image-height GAME-OVER-MSG-WALL))
  (render-ws
   (make-world-state
    (make-worm
     (list
      (make-posn -1 6)
      (make-posn 1 6)
      (make-posn 2 6)
      (make-posn 3 6)
      (make-posn 4 6)) "left")
    TEST-FOOD-2))))

(check-expect
 (show-game-over-msg
  (make-world-state
   (make-worm
    (list
     (make-posn 11 6)
     (make-posn 11 7)
     (make-posn 10 7)
     (make-posn 10 6)
     (make-posn 11 6)) "up")
   TEST-FOOD-2))
 (place-image
  GAME-OVER-MSG-WORM
  (- SCENE-WIDTH (/ (image-width GAME-OVER-MSG-WORM) 2))
  (- SCENE-HEIGHT (image-height GAME-OVER-MSG-WORM))
  (render-ws
   (make-world-state
    (make-worm
     (list
      (make-posn 11 6)
      (make-posn 11 7)
      (make-posn 10 7)
      (make-posn 10 6)
      (make-posn 11 6)) "up")
    TEST-FOOD-2))))

(define (show-game-over-msg ws)
  (cond
    [(worm-hit-itself? (world-state-worm ws))
     (place-image
      GAME-OVER-MSG-WORM
      (- SCENE-WIDTH (/ (image-width GAME-OVER-MSG-WORM) 2))
      (- SCENE-HEIGHT (image-height GAME-OVER-MSG-WORM))
      (render-ws ws))]
    [(worm-hit-the-wall? (world-state-worm ws))
     (place-image
      GAME-OVER-MSG-WALL
      (- SCENE-WIDTH (/ (image-width GAME-OVER-MSG-WORM) 2))
      (- SCENE-HEIGHT (image-height GAME-OVER-MSG-WALL))
      (render-ws ws))]))

; Worm -> Boolean

(check-expect (worm-hit-itself? (make-worm '() "up")) #false)
(check-expect (worm-hit-itself? TEST-WORM) #false)
(check-expect
 (worm-hit-itself?
  (make-worm
   (list
    (make-posn 11 6)
    (make-posn 11 7)
    (make-posn 10 7)
    (make-posn 10 6)
    (make-posn 11 6)) "up")) #true)

(define (worm-hit-itself? w)
  (cond
    [(empty? (worm-tail w)) #false]
    [else
     (member
      (first (worm-tail w)) (rest (worm-tail w)))]))

; Worm -> Boolean
; determines if the worm has hit the wall

(check-expect (worm-hit-the-wall? (make-worm '() "up")) #false)
(check-expect (worm-hit-the-wall? TEST-WORM) #false)
(check-expect
 (worm-hit-the-wall?
  (make-worm
   (list
    (make-posn -1 6)
    (make-posn 1 6)
    (make-posn 2 6)
    (make-posn 3 6)
    (make-posn 4 6)) "left")) #true)

(define (worm-hit-the-wall? w)
  (cond
    [(empty? (worm-tail w)) #false]
    [else
     (or
      (< (posn-x (first (worm-tail w))) 0)
      (< (posn-y (first (worm-tail w))) 0)
      (>
       (posn-x (first (worm-tail w)))
       (/ SCENE-WIDTH (* WORM-SEGMENT-SIZE 2)))
      (>
       (posn-y (first (worm-tail w)))
       (/ SCENE-WIDTH (* WORM-SEGMENT-SIZE 2))))]))

; WorldState Key -> WorldState
; produces the next world state

(define (control-ws ws ke)
  (make-world-state
   (direct-worm (world-state-worm ws) ke)
   (world-state-food ws)))

; Worm Key -> Worm
; sets the next direction of the worm based on the key ke

(check-expect
 (direct-worm TEST-WORM-LEFT "right") TEST-WORM-LEFT)
(check-expect
 (direct-worm TEST-WORM-UP "left") TEST-WORM-LEFT)

(define (direct-worm w ke)
  (cond
    [(string=? ke "left")
     (if (string=? (worm-direction w) "right")
         w
         (make-worm (worm-tail w) ke))]
    [(string=? ke "right")
     (if (string=? (worm-direction w) "left")
         w
         (make-worm (worm-tail w) ke))]
    [(string=? ke "up")
     (if (string=? (worm-direction w) "down")
         w
         (make-worm (worm-tail w) ke))]
    [(string=? ke "down")
     (if (string=? (worm-direction w) "up")
         w
         (make-worm (worm-tail w) ke))]
    [else w]))

; Posn -> Posn
; creates worm food
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
     p (make-posn
        (random
         (- (quotient SCENE-WIDTH (image-width WORM-SEGMENT)) 1))
        (random
         (- (quotient SCENE-HEIGHT (image-width WORM-SEGMENT)) 1)))))

; Posn Posn -> Posn
; generative recursion
; ensures candidate food is in a new location
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
