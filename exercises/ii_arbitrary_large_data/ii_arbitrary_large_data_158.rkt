;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_158) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 158.

(require 2htdp/universe)

(define HEIGHT 220) ; distances in terms of pixels 
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 5))
 
; graphical constants 
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT (rectangle 2 6 "solid" "red"))

; A List-of-shots is one of:
; - '()
; - (cons Shot List-of-shots
; interpretation the collection of short fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> ShotWorld
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))

; ShotWorld -> Image
; adds the image of a shot for each  y on w 
; at (MID,y} to the background image

(check-expect (to-image '()) BACKGROUND)

(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))

(check-expect
 (to-image (cons 9 (cons 20 '())))
 (place-image SHOT XSHOTS 9 (place-image SHOT XSHOTS 20 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else
     (place-image SHOT XSHOTS (first w) (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock '()) '())
(check-expect (tock (cons 50 '())) (cons 49 '()))
(check-expect (tock (cons 50 (cons 30 '())))
              (cons 49 (cons 29 '())))

(define (tock w)
  (cond
    [(empty? w) w]
    [else
     (eliminate (cons (sub1 (first w)) (tock (rest w))))]))

; ShotWorld -> ShotWorld
; eliminates the shots that are out of the convas
(check-expect (eliminate '()) '())
(check-expect (eliminate (cons -3 '())) '())
(check-expect (eliminate (cons 3 '())) (cons 3 '()))
(check-expect (eliminate (cons 2 (cons 0 (cons 3 (cons -1 '())))))
              (cons 2 (cons 3 '())))

(define (eliminate w)
  (cond
   [(empty? w) w]
   [(<= (first w) 0)
    (cond
      [(empty? (rest w)) '()]
      [else
       (cons (first (rest w)) (eliminate (rest (rest w))))])]
   [else
    (cons (first w) (eliminate (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar

(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh '() "a") '())
(check-expect
 (keyh (cons 10 '()) "h") (cons 10 '()))
(check-expect
 (keyh (cons 10 '()) " ") (cons HEIGHT (cons 10 '())))
(check-expect
 (keyh (cons 50 (cons 40 '())) " ")
 (cons HEIGHT (cons 50 (cons 40 '()))))

(define (keyh w ke)
  (cond
    [(string=? ke " ")
     (cond
       [(empty? w) (cons HEIGHT '())]
       [else (cons HEIGHT w)])]
    [else w]))